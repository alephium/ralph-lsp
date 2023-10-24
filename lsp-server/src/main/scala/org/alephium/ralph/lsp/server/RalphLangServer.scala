package org.alephium.ralph.lsp.server

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceChangeResult, WorkspaceFileEvent, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.alephium.ralph.lsp.server.RalphLangServer._
import org.alephium.ralph.lsp.server.converter.DiagnosticsConverter
import org.alephium.ralph.lsp.server.state.{ServerState, ServerStateUpdater}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.{messages, CompletableFutures}
import org.eclipse.lsp4j.services._

import java.net.URI
import java.util
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
        buildErrors = None
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
        buildErrors = None
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

  private def getClient(): RalphLangClient =
    state.client getOrElse {
      throw ResponseError.ClientNotConfigured.toResponseErrorException
    }

  private def setWorkspace(workspace: WorkspaceState): Unit =
    thisServer.synchronized {
      thisServer.state = thisServer.state.copy(workspace = Some(workspace))
    }

  private def setWorkspaceChange(fileURI: URI,
                                 changeResult: Option[WorkspaceChangeResult]): Iterable[PublishDiagnosticsParams] =
    thisServer.synchronized {
      changeResult match {
        case Some(result) =>
          setWorkspaceChange(result)

        case None =>
          // Means: This fileURI does not belong to this workspace or is of different type.
          // If this occurs, it's a client configuration result.
          // File types that are not supported by ralph should not be submitted to this server.
          val error = ResponseError.UnknownFileType(fileURI)
          logger.error(error.getMessage, error)
          getClient().log(error) // notify client
          throw error.toResponseErrorException
      }
    }

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

  /**
   * An initial call to this function is required before server is initialised.
   *
   * @param client   Client proxy instance provided by LSP4J.
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

  // TODO: If allowed in this phase (maybe? the doc seem to indicate no), access the PresentationCompiler
  //       and do an initial workspace compilation.
  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    CompletableFutures.computeAsync {
      cancelChecker =>
        // Previous commit uses the non-deprecated API but that does not work in vim.
        val rootURI =
          RalphLangServer.getRootUri(params)

        val workspaceURI =
          rootURI.getOrElse(throw ResponseError.WorkspaceFolderNotSupplied.toResponseErrorException)

        val workspace =
          Workspace.create(workspaceURI)

        setWorkspace(workspace)

        cancelChecker.checkCanceled()

        new InitializeResult(serverCapabilities())
    }

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val fileURI = new URI(params.getTextDocument.getUri)
    val code = Some(params.getTextDocument.getText)

    logger.debug(s"didOpen. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

    didChangeAndPublish(
      fileURI = fileURI,
      code = code
    )
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val fileURI = new URI(params.getTextDocument.getUri)
    val code = Some(params.getContentChanges.get(0).getText)

    logger.debug(s"didChange. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

    didChangeAndPublish(
      fileURI = fileURI,
      code = code
    )
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    val fileURI = new URI(params.getTextDocument.getUri)

    logger.debug(s"didClose. fileURI: $fileURI")

    didChangeAndPublish(
      fileURI = fileURI,
      code = None
    )
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    val fileURI = new URI(params.getTextDocument.getUri)
    val code = Some(params.getText)

    logger.debug(s"didSave. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

    didChangeAndPublish(
      fileURI = fileURI,
      code = code
    )
  }

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
    thisServer.synchronized {
      val changes =
        params.getChanges

      logger.debug(s"didChangeWatchedFiles: ${changes.asScala.mkString("\n", "\n", "")}")

      // collect Delete events
      val deleteEvents =
        changes.asScala collect {
          event =>
            event.getType match {
              case FileChangeType.Deleted =>
                WorkspaceFileEvent.Deleted(new URI(event.getUri))
            }
        }

      if (deleteEvents.nonEmpty) {
        val diagnostics =
          getOrBuildWorkspace() map { // initialise workspace and process delete
            source =>
              val deleteResult =
                Workspace.delete(
                  events = deleteEvents.to(ArraySeq),
                  buildErrors = thisServer.state.buildErrors,
                  workspace = source
                )

              setWorkspaceChange(deleteResult)
          }

        val client =
          getClient()

        diagnostics.merge foreach client.publishDiagnostics
      }
    }

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
   * Processes source or build file change.
   *
   * @param fileURI File that changed.
   * @param code    Content of the file.
   */
  private def didChangeAndSet(fileURI: URI,
                              code: Option[String]): Iterable[PublishDiagnosticsParams] =
    thisServer.synchronized {
      val diagnostics =
        getOrBuildWorkspace() map {
          workspace =>
            val result =
              Workspace.changed(
                fileURI = fileURI,
                code = code,
                currentWorkspace = workspace
              )

            setWorkspaceChange(
              fileURI = fileURI,
              changeResult = result
            )
        }

      diagnostics.merge
    }

  /**
   * Returns existing workspace or initialises a new one from the configured build file.
   * Or else reports any workspace issues.
   */
  def getOrBuildWorkspace(): Either[Iterable[PublishDiagnosticsParams], WorkspaceState.SourceAware] =
    thisServer.synchronized {
      getWorkspace() match {
        case sourceAware: WorkspaceState.SourceAware =>
          // already built
          Right(sourceAware)

        case currentWorkspace: WorkspaceState.Created =>
          // workspace is created but it's not built yet. Build it!
          val buildResult =
            Workspace.build(currentWorkspace)

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
              // Build passed. Set the workspace.
              // No need to build diagnostics here, the caller should, since it's requested
              // for this workspace for further compilation. The next compilation should publish diagnostics.
              setWorkspace(workspace)
              Right(workspace)
          }
      }
    }

  def getWorkspace(): WorkspaceState =
    state.workspace getOrElse {
      // Workspace folder is not defined.
      // This is not expected to occur since `initialized` is always invoked first.

      val error =
        ResponseError.WorkspaceFolderNotSupplied

      val exception =
        getClient()
          .log(error)
          .toResponseErrorException

      logger.error(error.getMessage, exception)

      throw exception
    }

  override def codeAction(params: CodeActionParams): CompletableFuture[util.List[messages.Either[Command, CodeAction]]] =
    CompletableFuture.completedFuture(util.Arrays.asList())

  override def resolveCompletionItem(unresolved: CompletionItem): CompletableFuture[CompletionItem] =
    CompletableFuture.completedFuture(unresolved)

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    ()

  override def getTextDocumentService: TextDocumentService =
    this

  override def getWorkspaceService: WorkspaceService =
    this

  override def shutdown(): CompletableFuture[AnyRef] =
    state.listener match {
      case Some(listener) =>
        CompletableFuture.supplyAsync {
          () =>
            java.lang.Boolean.valueOf(true)
        }

      case None =>
        CompletableFuture.failedFuture(new Exception("Listener not set"))
    }

  override def exit(): Unit =
    System.exit(0)

}
