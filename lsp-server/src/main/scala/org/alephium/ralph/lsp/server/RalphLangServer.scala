package org.alephium.ralph.lsp.server

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.completion.CodeCompleter
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, WorkspaceBuild}
import org.alephium.ralph.lsp.server.RalphLangServer._
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
    capabilities.setDiagnosticProvider(new DiagnosticRegistrationOptions(true, true))

    capabilities
  }

  /** Start server with pre-configured client */
  def apply(client: RalphLangClient,
            listener: JFuture[Void])(implicit compiler: CompilerAccess): RalphLangServer = {
    val initialState =
      ServerState(
        client = Some(client),
        listener = Some(listener),
        workspace = None,
        buildErrors = None
      )

    new RalphLangServer(initialState)
  }

  def apply()(implicit compiler: CompilerAccess): RalphLangServer =
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
class RalphLangServer private(@volatile private var state: ServerState)(implicit compiler: CompilerAccess) extends LanguageServer with TextDocumentService with WorkspaceService with StrictLogging {

  def getState(): ServerState =
    this.state

  private def getClient(): RalphLangClient =
    state.client getOrElse {
      throw ResponseError.ClientNotConfigured.toResponseErrorException
    }

  private def setWorkspace(workspace: WorkspaceState): Unit =
    this.synchronized {
      this.state = this.state.copy(workspace = Some(workspace))
    }

  /** Publish build file change result */
  private def setAndPublishBuildChange(currentWorkspace: WorkspaceState,
                                       buildChangeResult: Either[BuildState.BuildErrored, WorkspaceState]): Unit =
    buildChangeResult match {
      case Left(buildError) =>
        setAndPublishBuild(
          currentWorkspace = currentWorkspace,
          buildError = buildError
        )

      case Right(newWorkspace) =>
        setAndPublishBuild(
          currentWorkspace = currentWorkspace,
          newWorkspace = newWorkspace
        )
    }

  /** Publish source-code change result */
  private def setAndPublishSourceCodeChange(currentWorkspace: WorkspaceState,
                                            sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState]): Unit =
    sourceChangeResult match {
      case Left(buildError) =>
        setAndPublishBuild(
          currentWorkspace = currentWorkspace,
          buildError = buildError
        )

      case Right(newWorkspace) =>
        setAndPublishWorkspace(
          currentWorkspace = currentWorkspace,
          newWorkspace = newWorkspace
        )
    }

  /** Publish the build errors */
  private def setAndPublishBuild(currentWorkspace: WorkspaceState,
                                 buildError: BuildState.BuildErrored): Unit =
    this.synchronized {
      getClient().publishErrors(
        fileURI = buildError.buildURI,
        code = buildError.code,
        errors = buildError.errors.toList
      )

      this.state = this.state.copy(buildErrors = Some(buildError))
    }

  /** Build is successfully compiled on a workspace. Publish the workspace, clearly existing build-error messages. */
  private def setAndPublishBuild(currentWorkspace: WorkspaceState,
                                 newWorkspace: WorkspaceState): Unit =
    this.synchronized {
      this.state.buildErrors foreach {
        build =>
          // clear existing build errors
          getClient().publishErrors(
            fileURI = build.buildURI,
            code = build.code,
            errors = List.empty
          )
      }

      // clear the build errors from state
      this.state = this.state.copy(buildErrors = None)

      // publish the new-workspace that is built with the new-build-file.
      setAndPublishWorkspace(
        currentWorkspace = currentWorkspace,
        newWorkspace = newWorkspace
      )
    }

  /** Publish new workspace */
  private def setAndPublishWorkspace(currentWorkspace: WorkspaceState,
                                     newWorkspace: WorkspaceState): Unit =
    this.synchronized {
      // New valid workspace created. Set it!
      setWorkspace(newWorkspace)

      (currentWorkspace, newWorkspace) match {
        case (_: WorkspaceState.Created, newWorkspace: WorkspaceState.SourceAware) =>
          // publish first compilation result i.e. previous workspace had no compilation run.
          getClient().publish(
            currentWorkspace = newWorkspace,
            newWorkspace = None
          )

        case (currentWorkspace: WorkspaceState.SourceAware, newWorkspace: WorkspaceState.SourceAware) =>
          // publish new workspace given previous workspace.
          getClient().publish(
            currentWorkspace = currentWorkspace,
            newWorkspace = Some(newWorkspace)
          )

        case (_, _: WorkspaceState.Created) =>
          // Nothing to publish
          ()
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
    this.synchronized {
      require(state.client.isEmpty, "Client is already set")
      require(state.listener.isEmpty, "Listener is already set")

      // Client must be set first, before running the request listener,
      // so that it is available for responding to requests.
      this.state = state.copy(client = Some(client))
      this.state = state.copy(listener = Some(listener()))
    }

  // TODO: If allowed in this phase (maybe? the doc seem to indicate no), access the PresentationCompiler
  //       and do an initial workspace compilation.
  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    CompletableFutures.computeAsync {
      cancelChecker =>
        // Previous commit uses the non-deprecated API but that does not work in vim.
        val rootURI = RalphLangServer.getRootUri(params)

        val workspaceURI =
          rootURI.getOrElse(throw ResponseError.WorkspaceFolderNotSupplied.toResponseErrorException)

        val workspace =
          Workspace.create(workspaceURI)

        setWorkspace(workspace)

        cancelChecker.checkCanceled()

        new InitializeResult(serverCapabilities())
    }

  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    didChange(
      fileURI = new URI(params.getTextDocument.getUri),
      code = Some(params.getTextDocument.getText)
    )

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    didChange(
      fileURI = new URI(params.getTextDocument.getUri),
      code = Some(params.getContentChanges.get(0).getText)
    )

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    didChange(
      fileURI = new URI(params.getTextDocument.getUri),
      code = None
    )

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    ()

  /**
   * Processes source or build file change.
   *
   * @param fileURI File that changed.
   * @param code    Content of the file.
   */
  def didChange(fileURI: URI,
                code: Option[String]): Unit =
    this.synchronized {
      val currentWorkspace =
        getWorkspace()

      val fileExtension =
        URIUtil.getFileExtension(fileURI)

      if (fileExtension == WorkspaceBuild.BUILD_FILE_EXTENSION) {
        // process build change
        val buildResult =
          Workspace.buildChanged(
            fileURI = fileURI,
            code = code,
            workspace = currentWorkspace
          )

        setAndPublishBuildChange(
          currentWorkspace = currentWorkspace,
          buildChangeResult = buildResult
        )
      } else if (fileExtension == CompilerAccess.RALPH_FILE_EXTENSION) {
        // process source code change
        val sourceResult =
          Workspace.sourceCodeChanged(
            fileURI = fileURI,
            updatedCode = code,
            workspace = currentWorkspace
          )

        setAndPublishSourceCodeChange(
          currentWorkspace = currentWorkspace,
          sourceChangeResult = sourceResult
        )
      } else {
        // If this occurs, it's a client configuration error.
        // File types that are not supported by ralph should not be submitted to this server.
        val error = ResponseError.UnknownFileType(fileURI)
        logger.error(error.getMessage, error)
        getClient().log(error) // notify client
        throw error.toResponseErrorException
      }
    }

  override def completion(params: CompletionParams): CompletableFuture[messages.Either[util.List[CompletionItem], CompletionList]] =
    CompletableFutures.computeAsync {
      cancelChecker =>
        val fileURI = new URI(params.getTextDocument.getUri)
        val line = params.getPosition.getLine
        val character = params.getPosition.getCharacter

        cancelChecker.checkCanceled()

        val workspace = getOrInitWorkspace()

        val suggestions =
          CodeCompleter.complete(
            line = line,
            character = character,
            uri = fileURI,
            workspace = workspace
          )

        val completionList =
          DataConverter.toCompletionList(suggestions)

        cancelChecker.checkCanceled()

        messages.Either.forRight[util.List[CompletionItem], CompletionList](completionList)
    }

  /**
   * Returns existing workspace or initialises a new one from the configured build file.
   * Or else reports any workspace issues.
   */
  def getOrInitWorkspace(): WorkspaceState.SourceAware =
    this.synchronized { // TODO: Remove synchronized. Use async.
      getWorkspace() match {
        case sourceAware: WorkspaceState.SourceAware =>
          // already built
          sourceAware

        case currentWorkspace: WorkspaceState.Created =>
          // perform build and bring workspace state to unCompiled
          val newWorkspace =
            Workspace.initialise(currentWorkspace)

          setAndPublishSourceCodeChange(
            currentWorkspace = currentWorkspace,
            sourceChangeResult = newWorkspace
          )

          newWorkspace getOrElse {
            throw
              ResponseError
                .UnableToInitialiseWorkspace
                .toResponseErrorException
          }
      }
    }

  def getWorkspace(): WorkspaceState =
    state.workspace match {
      case Some(workspace) =>
        workspace

      case None =>
        // Workspace folder is not defined.
        // This is not expected to occur since `initialized` is always invoked first.
        throw
          getClient()
            .log(ResponseError.WorkspaceFolderNotSupplied)
            .toResponseErrorException
    }

  override def diagnostic(params: WorkspaceDiagnosticParams): CompletableFuture[WorkspaceDiagnosticReport] =
    CompletableFuture.completedFuture(new WorkspaceDiagnosticReport(util.Arrays.asList()))

  override def diagnostic(params: DocumentDiagnosticParams): CompletableFuture[DocumentDiagnosticReport] =
    CompletableFuture.completedFuture(new DocumentDiagnosticReport(new RelatedFullDocumentDiagnosticReport(util.Arrays.asList())))

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
