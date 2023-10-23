package org.alephium.ralph.lsp.server

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.completion.CodeCompleter
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceChangeResult, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.alephium.ralph.lsp.server.RalphLangServer._
import org.alephium.ralph.lsp.server.converter.{CompletionConverter, DiagnosticsConverter}
import org.alephium.ralph.lsp.server.state.{ServerState, ServerStateUpdater}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.{messages, CompletableFutures}
import org.eclipse.lsp4j.services._

import java.net.URI
import java.util
import java.util.concurrent.{CompletableFuture, Future => JFuture}

object RalphLangServer {

  /** Build capabilities supported by the LSP server */
  def serverCapabilities(): ServerCapabilities = {
    val capabilities = new ServerCapabilities()

    capabilities.setCompletionProvider(new CompletionOptions(true, util.Arrays.asList(".")))
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
          // If this occurs, it's a client configuration error.
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

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    ()

  private def didChangeAndPublish(fileURI: URI,
                                  code: Option[String]): Unit =
    thisServer.synchronized {
      val client = getClient()

      didChangeAndSet(
        fileURI = fileURI,
        code = code
      ) foreach client.publishDiagnostics
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
      val result =
        Workspace.changed(
          fileURI = fileURI,
          code = code,
          currentWorkspace = getWorkspace()
        )

      setWorkspaceChange(
        fileURI = fileURI,
        changeResult = result
      )
    }

  override def completion(params: CompletionParams): CompletableFuture[messages.Either[util.List[CompletionItem], CompletionList]] =
    CompletableFutures.computeAsync {
      cancelChecker =>
        val fileURI = new URI(params.getTextDocument.getUri)
        val line = params.getPosition.getLine
        val character = params.getPosition.getCharacter

        cancelChecker.checkCanceled()

        val workspace =
          getOrInitWorkspace() getOrElse {
            throw
              ResponseError
                .UnableToInitialiseWorkspace
                .toResponseErrorException
          }

        val suggestions =
          CodeCompleter.complete(
            line = line,
            character = character,
            uri = fileURI,
            workspace = workspace
          )

        val completionList =
          CompletionConverter.toCompletionList(suggestions)

        cancelChecker.checkCanceled()

        messages.Either.forRight[util.List[CompletionItem], CompletionList](completionList)
    }

  /**
   * Returns existing workspace or initialises a new one from the configured build file.
   * Or else reports any workspace issues.
   */
  def getOrInitWorkspace(): Either[BuildState.BuildErrored, WorkspaceState.SourceAware] =
    thisServer.synchronized {
      getWorkspace() match {
        case sourceAware: WorkspaceState.SourceAware =>
          // already built
          Right(sourceAware)

        case currentWorkspace: WorkspaceState.Created =>
          // perform build and bring workspace state to unCompiled
          val newWorkspace =
            Workspace.initialise(currentWorkspace)

          val client =
            getClient()

          setWorkspaceChange(
            changeResult = WorkspaceChangeResult.BuildChanged(Some(newWorkspace))
          ) foreach client.publishDiagnostics

          newWorkspace
      }
    }

  def getWorkspace(): WorkspaceState =
    state.workspace getOrElse {
      // Workspace folder is not defined.
      // This is not expected to occur since `initialized` is always invoked first.
      throw
        getClient()
          .log(ResponseError.WorkspaceFolderNotSupplied)
          .toResponseErrorException
    }

  override def codeAction(params: CodeActionParams): CompletableFuture[util.List[messages.Either[Command, CodeAction]]] =
    CompletableFuture.completedFuture(util.Arrays.asList())

  override def resolveCompletionItem(unresolved: CompletionItem): CompletableFuture[CompletionItem] =
    CompletableFuture.completedFuture(unresolved)

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    ()

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
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
            java.lang.Boolean.valueOf(listener.cancel(true))
        }

      case None =>
        CompletableFuture.failedFuture(new Exception("Listener not set"))
    }

  override def exit(): Unit =
    System.exit(0)

}
