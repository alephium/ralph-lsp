package org.alephium.ralph.lsp.server

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.completion.CodeCompleter
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, WorkspaceBuild}
import org.alephium.ralph.lsp.server.RalphLangServer._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.{messages, CompletableFutures, ResponseErrorException}
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
        workspace = None
      )

    new RalphLangServer(initialState)
  }
}

/**
 * The Ralph-LSP server.
 *
 * This class is the only one with mutable state in this repo.
 * All mutable state management occurs here.
 */
class RalphLangServer(@volatile private var state: ServerState = ServerState(None, None, None))(implicit compiler: CompilerAccess) extends LanguageServer with TextDocumentService with WorkspaceService with StrictLogging {

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

  /**
   * Publish workspace messages given it's previous states in order.
   *
   * Previous states are used to clear error/warnings that are fixed in newer states.
   *
   * @param head First workspace state
   * @param tail Newer states following the first state.
   *             Set this to empty if head is the only state.
   */
  private def setAndPublishWorkspace(head: WorkspaceState.SourceAware,
                                     tail: WorkspaceState.SourceAware*): Unit =
    this.synchronized {
      val last = tail.lastOption.getOrElse(head)
      setWorkspace(last)
      getClient().publish(head, tail)
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
        val rootURI =
          Option(params.getRootUri)
            .getOrElse(params.getRootPath)

        val workspaceURI =
          if (rootURI == null)
            throw ResponseError.WorkspaceFolderNotSupplied.toResponseErrorException
          else
            new URI(rootURI)

        val workspace =
          Workspace.create(workspaceURI)

        setWorkspace(workspace)

        cancelChecker.checkCanceled()

        new InitializeResult(serverCapabilities())
    }

  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    didCodeChange(
      fileURI = new URI(params.getTextDocument.getUri),
      code = Some(params.getTextDocument.getText)
    )

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    didCodeChange(
      fileURI = new URI(params.getTextDocument.getUri),
      code = Some(params.getContentChanges.get(0).getText)
    )

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    didCodeChange(
      fileURI = new URI(params.getTextDocument.getUri),
      code = None
    )

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    didCodeChange(
      fileURI = new URI(params.getTextDocument.getUri),
      code = None
    )

  /**
   * [[Workspace]] reacts to all code/build changes the same.
   *
   * @param fileURI File that changed.
   * @param code    Content of the file.
   */

  def didCodeChange(fileURI: URI,
                    code: Option[String]): Unit = {
    val fileExtension = URIUtil.getFileExtension(fileURI)
    if (fileExtension == WorkspaceBuild.BUILD_FILE_EXTENSION)
      buildChanged(
        fileURI = fileURI,
        code = code
      )
    else if (fileExtension == CompilerAccess.RALPH_FILE_EXTENSION)
      codeChanged(
        fileURI = fileURI,
        updatedCode = code
      )
    else
      throw
        getClient()
          .log(ResponseError.UnknownFile(fileURI))
          .toResponseErrorException
  }

  /**
   * Handles changes to the build valid.
   *
   * If the build file is valid, this drops existing compilations
   * and starts a fresh workspace.
   *
   * @param fileURI Location of the build file.
   * @param code    Build file's content.
   */
  def buildChanged(fileURI: URI,
                   code: Option[String]): Unit =
    this.synchronized {
      val fileName = URIUtil.getFileName(fileURI)

      if (fileName == WorkspaceBuild.BUILD_FILE_NAME)
        Workspace.build(
          buildURI = fileURI,
          build = code,
          state = getWorkspace()
        ) match {
          case Some(buildState: BuildState) =>
            initialiseWorkspace(buildState)

          case None =>
            // Build file did not change. Clear any older error messages.
            getClient().publishErrors(
              fileURI = fileURI,
              code = code,
              errors = List.empty
            )
        }
      else
        throw
          getClient()
            .log(ResponseError.InvalidBuildFileName(fileName))
            .toResponseErrorException
    }

  /** Creates an un-compiled workspace for a successful build file. */
  def initialiseWorkspace(state: BuildState): WorkspaceState.UnCompiled =
    this.synchronized {
      state match {
        case compiled: BuildState.BuildCompiled =>
          // Build file changed. Update the workspace and request a full workspace build.
          Workspace.initialise(compiled) match {
            case Left(error) =>
              // Project level error: Failed to initialise workspace.
              throw getClient().log(error)

            case Right(unCompiledWorkspace) =>
              // set the new workspace and publish diagnostics
              setAndPublishWorkspace(unCompiledWorkspace)

              // clear any build errors
              getClient().publishErrors(
                fileURI = unCompiledWorkspace.build.buildURI,
                code = Some(unCompiledWorkspace.build.code),
                errors = List.empty
              )

              // request project wide re-build TODO: Handle future
              getClient().refreshDiagnostics()
              unCompiledWorkspace
          }

        case errored: BuildState.BuildErrored =>
          getClient().publishErrors(
            fileURI = errored.buildURI,
            code = errored.code,
            errors = errored.errors.toList
          )

          // TODO
          errored.errors.headOption match {
            case Some(firstError) =>
              throw firstError

            case None =>
              ???
          }
      }
    }

  /**
   * Handles changes to ralph code files.
   *
   * @param fileURI     Location of the source file.
   * @param updatedCode Source changes
   */
  def codeChanged(fileURI: URI,
                  updatedCode: Option[String]): Unit =
    this.synchronized { // TODO: Remove synchronized. Use async.
      val workspace = getOrInitWorkspace()

      val codeChangedWorkspace =
        Workspace.codeChanged(
          fileURI = fileURI,
          updatedCode = updatedCode,
          currentState = workspace
        )

      // immediately set the new state with the updated code so it is available for other threads.
      setWorkspace(codeChangedWorkspace)

      // compile the new state.
      val compiledWorkspace =
        Workspace.parseAndCompile(codeChangedWorkspace)

      // set and publish the compiled state
      setAndPublishWorkspace(codeChangedWorkspace, compiledWorkspace)
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
   *
   * @note Does not update the current state. The caller should set the new state.
   */
  def getOrInitWorkspace(): WorkspaceState.SourceAware =
    this.synchronized { // TODO: Remove synchronized. Use async.
      state.workspace match {
        case Some(aware: WorkspaceState.SourceAware) =>
          aware

        case Some(initialised: WorkspaceState.Initialised) =>
          val newBuild =
            Workspace.build(
              code = None,
              state = initialised
            )

          initialiseWorkspace(newBuild)

        case None =>
          throw reportMissingWorkspace()
      }
    }

  def getWorkspace(): WorkspaceState =
    state.workspace match {
      case Some(workspace) =>
        workspace

      case None =>
        throw reportMissingWorkspace()
    }

  // Workspace folder is not defined.
  // This is not expected to occur since `initialized` is always invoked first.
  def reportMissingWorkspace(): ResponseErrorException =
    getClient()
      .log(ResponseError.WorkspaceFolderNotSupplied)
      .toResponseErrorException

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    ()

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
    ()

  override def getTextDocumentService: TextDocumentService =
    this

  override def getWorkspaceService: WorkspaceService =
    this

  override def diagnostic(params: WorkspaceDiagnosticParams): CompletableFuture[WorkspaceDiagnosticReport] =
    CompletableFuture.completedFuture(new WorkspaceDiagnosticReport(util.Arrays.asList()))

  override def diagnostic(params: DocumentDiagnosticParams): CompletableFuture[DocumentDiagnosticReport] =
    CompletableFuture.completedFuture(new DocumentDiagnosticReport(new RelatedFullDocumentDiagnosticReport(util.Arrays.asList())))

  override def codeAction(params: CodeActionParams): CompletableFuture[util.List[messages.Either[Command, CodeAction]]] =
    CompletableFuture.completedFuture(util.Arrays.asList())

  override def resolveCompletionItem(unresolved: CompletionItem): CompletableFuture[CompletionItem] =
    CompletableFuture.completedFuture(unresolved)

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
