package org.alephium.ralph.lsp.server

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace._
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorUnknownFileType
import org.alephium.ralph.lsp.server
import org.alephium.ralph.lsp.server.RalphLangServer._
import org.alephium.ralph.lsp.server.converter.DiagnosticsConverter
import org.alephium.ralph.lsp.server.state.{ServerState, ServerStateUpdater}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import org.eclipse.lsp4j.services._

import java.net.URI
import java.util.concurrent.{CompletableFuture, Future => JFuture}
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.IterableHasAsScala

object RalphLangServer {

  /** Build capabilities supported by the LSP server */
  def serverCapabilities(): ServerCapabilities = {
    val capabilities = new ServerCapabilities()

    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)

    capabilities
  }

  /** Start server with pre-configured client */
  def apply(client: RalphLangClient,
            listener: JFuture[Void])(implicit compiler: CompilerAccess,
                                     file: FileAccess): RalphLangServer = {
    val initialState =
      ServerState(
        client = Some(client),
        listener = Some(listener),
        workspace = None,
        buildErrors = None,
        clientCapabilities = None,
        shutdownReceived = false
      )

    new RalphLangServer(initialState)
  }

  def apply()(implicit compiler: CompilerAccess,
              file: FileAccess): RalphLangServer =
    new RalphLangServer(
      ServerState(
        client = None,
        listener = None,
        workspace = None,
        buildErrors = None,
        clientCapabilities = None,
        shutdownReceived = false
      )
    )

  def getRootUri(params: InitializeParams): Option[URI] =
    Option(params.getRootUri)
      .orElse(Option(params.getRootPath))
      //Some LSP clients aren't providing `rootUri` or `rootPath`, like in nvim, so we fall back on `user.dir`
      .orElse(Option(System.getProperty("user.dir")).map(dir => s"file://$dir"))
      .map(new URI(_))
}

/**
 * The Ralph-LSP server.
 *
 * This class is the only one with mutable state in this repo.
 * All mutable state management occurs here.
 */
class RalphLangServer private(@volatile private var state: ServerState)(implicit compiler: CompilerAccess,
                                                                        file: FileAccess) extends LanguageServer with TextDocumentService with WorkspaceService with StrictLogging { thisServer =>

  def getState(): ServerState =
    thisServer.state

  /**
   * An initial call to this function is required before this server can start processing request.
   *
   * @param client   Client proxy instance provided by LSP4J.
   *                 Client must be known before a connection is initialised.
   * @param listener LSP connection listener function.
   */
  def setInitialState(client: RalphLangClient,
                      listener: () => JFuture[Void]): Unit =
    thisServer.synchronized {
      require(state.client.isEmpty, "Client is already set")
      require(state.listener.isEmpty, "Listener is already set")

      // Client must be set first, before running the request listener,
      // so that it is available for responding to requests.
      thisServer.state = state.copy(client = Some(client))
      thisServer.state = state.copy(listener = Some(listener()))
    }

  /** @inheritdoc */
  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    CompletableFutures.computeAsync {
      cancelChecker =>
        logger.debug("Initialize request")
        // Previous commit uses the non-deprecated API but that does not work in vim.
        val rootURI =
          RalphLangServer.getRootUri(params)

        val workspaceURI =
          rootURI getOrElse notifyAndThrow(ResponseError.WorkspaceFolderNotSupplied)

        val workspace =
          Workspace.create(workspaceURI)

        setWorkspace(workspace)

        setClientCapabilities(params.getCapabilities())

        cancelChecker.checkCanceled()

        new InitializeResult(serverCapabilities())
    }

  /** @inheritdoc */
  override def initialized(params: InitializedParams): Unit = {
    logger.debug("Client initialized")
    thisServer.state.clientCapabilities match {
      case Some(capabilities) =>
        val maybeDynamicRegistration: Option[Boolean] = for {
          workspace <- Option(capabilities.getWorkspace())
          didChangeWatchedFiles <- Option(workspace.getDidChangeWatchedFiles())
          dynamicRegistration <- Option(didChangeWatchedFiles.getDynamicRegistration())
        } yield dynamicRegistration

        if(maybeDynamicRegistration.getOrElse(false)) {
          logger.debug("Register watched files")
          getClient().registerWatchedFiles().whenComplete { case (_, error) =>
            if(error != null) {
              logger.error("Failed to register watched files", error)
            }
          }
        } else {
          logger.debug("Client doesn't support dynamic registration for watched files")
        }
      case None => ()
    }

    // Invoke initial compilation. Trigger it as build file changed.
    didChangeAndPublish(
      fileURI = getWorkspace().buildURI,
      code = None
    )
  }

  /** @inheritdoc */
  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val fileURI = new URI(params.getTextDocument.getUri)
    val code = Option(params.getTextDocument.getText)

    logger.debug(s"didOpen. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

    didChangeAndPublish(
      fileURI = fileURI,
      code = code
    )
  }

  /** @inheritdoc */
  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val fileURI = new URI(params.getTextDocument.getUri)
    val code = Option(params.getContentChanges.get(0).getText)

    logger.debug(s"didChange. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

    didChangeAndPublish(
      fileURI = fileURI,
      code = code
    )
  }

  /** @inheritdoc */
  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    val fileURI = new URI(params.getTextDocument.getUri)

    logger.debug(s"didClose. fileURI: $fileURI")

    didChangeAndPublish(
      fileURI = fileURI,
      code = None
    )
  }

  /** @inheritdoc */
  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    val fileURI = new URI(params.getTextDocument.getUri)
    val code = Option(params.getText)

    logger.debug(s"didSave. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

    didChangeAndPublish(
      fileURI = fileURI,
      code = code
    )
  }

  /** @inheritdoc */
  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
    thisServer.synchronized {
      val changes =
        params.getChanges

      logger.debug(s"didChangeWatchedFiles: ${changes.asScala.mkString("\n", "\n", "")}")

      // collect events
      val events =
        changes.asScala collect {
          event =>
            event.getType match {
              case FileChangeType.Deleted =>
                WorkspaceFileEvent.Deleted(new URI(event.getUri))

              case FileChangeType.Created =>
                WorkspaceFileEvent.Created(new URI(event.getUri))
            }
        }

      if (events.nonEmpty) {
        val diagnostics =
          getOrBuildWorkspace(None) map { // build workspace
            source =>
              // Build OK! process delete or create
              val deleteResult =
                Workspace.deleteOrCreate(
                  events = events.to(ArraySeq),
                  buildErrors = thisServer.state.buildErrors,
                  workspace = source
                )

              // Set the updated workspace
              setWorkspaceChange(deleteResult)
          }

        val client =
          getClient()

        diagnostics.merge foreach client.publishDiagnostics
      }
    }

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    ()

  override def getTextDocumentService: TextDocumentService =
    this

  override def getWorkspaceService: WorkspaceService =
    this

  /**
   * Apply code change and publish diagnostics.
   *
   * @param fileURI File that changed
   * @param code    Source-code of the changed file.
   */
  private def didChangeAndPublish(fileURI: URI,
                                  code: Option[String]): Unit =
    thisServer.synchronized {
      val diagnostics =
        didChangeAndSet(
          fileURI = fileURI,
          code = code
        )

      val client = getClient()

      diagnostics foreach client.publishDiagnostics
    }

  /**
   * Process code change and set the new workspace.
   *
   * @param fileURI File that changed.
   * @param code    Source-code of the changed file.
   * @return Diagnostics of the new workspace.
   */
  private def didChangeAndSet(fileURI: URI,
                              code: Option[String]): Iterable[PublishDiagnosticsParams] =
    thisServer.synchronized {
      val source =
        Some(WorkspaceFile(fileURI, code))

      val diagnostics =
        getOrBuildWorkspace(source) map {
          workspace =>
            val changeResult =
              Workspace.changed(
                fileURI = fileURI,
                code = code,
                currentWorkspace = workspace
              )

            setWorkspaceChange(changeResult)
        }

      diagnostics.merge
    }

  /**
   * Fetch the existing workspace if it's already build and initialised or-else invoke new workspace build.
   *
   * @param code File that changed and it's source-code.
   * @return Diagnostics if there were build errors, or-else the next workspace.
   */
  def getOrBuildWorkspace(code: Option[WorkspaceFile]): Either[Iterable[PublishDiagnosticsParams], WorkspaceState.IsSourceAware] =
    thisServer.synchronized {
      val workspace =
        getWorkspace()

      val buildResult =
        Workspace.build(
          code = code,
          workspace = workspace
        )

      // process build result
      buildResult match {
        case Left(error) =>
          // build errored
          val buildErrored =
            WorkspaceChangeResult.BuildChanged(Some(Left(error)))

          // set the build error and return diagnostics
          val diagnostics =
            setWorkspaceChange(changeResult = buildErrored)

          Left(diagnostics)

        case Right(workspace) =>
          // Build passed.
          // No need to build diagnostics or set the state here, the caller should,
          // because the caller this workspace is requested for further compilation.
          Right(workspace)
      }
    }

  private def getWorkspace(): WorkspaceState =
    state.workspace getOrElse {
      // Workspace folder is not defined.
      // This is not expected to occur since `initialized` is always invoked first.
      notifyAndThrow(ResponseError.WorkspaceFolderNotSupplied)
    }

  private def getClient(): RalphLangClient =
    state.client getOrElse {
      val error = ResponseError.ClientNotConfigured
      val exception = error.toResponseErrorException
      logger.error(error.getMessage, exception)
      throw exception
    }

  /** Set the workspace */
  private def setWorkspace(workspace: WorkspaceState): Unit =
    thisServer.synchronized {
      thisServer.state = thisServer.state.copy(workspace = Some(workspace))
    }

  private def setClientCapabilities(clientCapabilities: ClientCapabilities): Unit =
    thisServer.synchronized {
      thisServer.state = thisServer.state.copy(clientCapabilities = Some(clientCapabilities))
    }

  /**
   * Set the workspace and returns diagnostics to publish for current state.
   *
   * @param changeResult Compilation result returned by presentation-compiler.
   * @return Diagnostics for current workspace.
   */
  private def setWorkspaceChange(changeResult: Either[ErrorUnknownFileType, WorkspaceChangeResult]): Iterable[PublishDiagnosticsParams] =
    thisServer.synchronized {
      changeResult match {
        case Right(result) =>
          setWorkspaceChange(result)

        case Left(ErrorUnknownFileType(fileURI)) =>
          // Means: This fileURI does not belong to this workspace or is of different type.
          // If this occurs, it's a client configuration error.
          // File types that are not supported by Ralph should not be submitted to this server.
          notifyAndThrow(ResponseError.UnknownFileType(fileURI))
      }
    }

  /**
   * Set the workspace and returns diagnostics to publish for current state.
   *
   * @param changeResult Compilation result returned by presentation-compiler.
   *                     [[None]] indicates that the file-type does not belong to us.
   * @return Diagnostics for current workspace.
   */
  private def setWorkspaceChange(changeResult: WorkspaceChangeResult): Iterable[PublishDiagnosticsParams] =
    thisServer.synchronized {
      val currentServerState =
        thisServer.state

      val newServerState =
        ServerStateUpdater.workspaceChanged(
          change = changeResult,
          serverState = currentServerState
        )

      newServerState match {
        case Some(newState) =>
          thisServer.state = newState

          DiagnosticsConverter.toPublishDiagnostics(
            currentState = currentServerState,
            newState = newState
          )

        case None =>
          logger.debug("No server change occurred")
          None
      }
    }

  /** Write to log file, notify the client and throw to exit this request */
  private def notifyAndThrow(error: server.ResponseError): Nothing = {
    val client = getClient()
    val exception = client.log(error).toResponseErrorException
    logger.error(error.getMessage, exception)
    throw exception
  }

  /** Write to log file and send the error to the client */
  private def logAndSend[A](error: server.ResponseError): CompletableFuture[A] = {
    logger.error(error.getMessage, error.toResponseErrorException)
    val result = new CompletableFuture[A]()
    result.completeExceptionally(error.toResponseErrorException)
    result
  }

  override def shutdown(): CompletableFuture[AnyRef] =
    thisServer.synchronized {
      logger.info("shutdown")
      if(thisServer.state.shutdownReceived){
        logAndSend(ResponseError.ShutdownRequested)
      } else {
        thisServer.state = thisServer.state.copy(shutdownReceived = true)
        CompletableFuture.completedFuture(java.lang.Boolean.TRUE)
      }
    }

  def exitWithCode(): Int =
    thisServer.synchronized {
      logger.info("exit")

      thisServer.state.listener match {
        case Some(listener) =>
          listener.cancel(true)

        case None =>
          logger.error("Listener is empty. Exit invoked on server that is not initialised")
      }

      if(thisServer.state.shutdownReceived) {
        0
      } else {
        1
      }
    }

  override def exit(): Unit =
    System.exit(exitWithCode())
}
