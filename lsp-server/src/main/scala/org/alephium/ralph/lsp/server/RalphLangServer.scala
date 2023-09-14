package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.PresentationCompiler
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.server.RalphLangServer._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.{messages, CompletableFutures}
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode
import org.eclipse.lsp4j.services._

import java.net.URI
import java.util
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.CollectionHasAsScala

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
  def apply(client: RalphLangClient)(implicit compiler: CompilerAccess): RalphLangServer = {
    val server = new RalphLangServer()
    server.setClient(client)
    server
  }

}

/**
 * The Ralph-LSP server.
 *
 * This class is the only one with mutable state in this repo.
 * All mutable state management occurs here.
 */
class RalphLangServer(@volatile private var state: ServerState = ServerState())(implicit compiler: CompilerAccess) extends LanguageServer with TextDocumentService with WorkspaceService {

  def getState(): ServerState =
    this.state

  private def setState(state: ServerState): ServerState =
    this.synchronized {
      this.state = state

      // let the client know of workspace changes
      state.withClient {
        implicit client =>
          RalphLangClient.publish(state.workspace)
      }

      state
    }

  /**
   * Follows the same [[LanguageClientAware.connect]].
   *
   * Only mutable function available to the outside world.
   *
   * @param client client-proxy used to communicate with the client.
   */
  def setClient(client: RalphLangClient): Unit =
    setState(state.copy(client = Some(client)))

  // TODO: If allowed in this phase (maybe? the doc seem to indicate no), access the PresentationCompiler
  //       and do an initial workspace compilation.
  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    CompletableFutures.computeAsync {
      cancelChecker =>
        val workspaceFolders =
          params.getWorkspaceFolders.asScala

        val workspaceURI =
          if (workspaceFolders.isEmpty)
            throw RalphLangClient.responseError(
              errorCode = ResponseErrorCode.InvalidParams,
              message = "Workspace folder not supplied"
            )
          else if (workspaceFolders.size > 1)
            throw RalphLangClient.responseError(
              errorCode = ResponseErrorCode.InvalidParams,
              message = "Multiple root workspace folders are not supported"
            )
          else
            new URI(workspaceFolders.head.getUri)

        val workspace =
          PresentationCompiler.createWorkspace(workspaceURI)

        setState(state.updateWorkspace(workspace))

        cancelChecker.checkCanceled()

        new InitializeResult(serverCapabilities())
    }

  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    didCodeChange(
      fileURI = new URI(params.getTextDocument.getUri),
      updatedCode = Some(params.getTextDocument.getText)
    )

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    didCodeChange(
      fileURI = new URI(params.getTextDocument.getUri),
      updatedCode = Some(params.getContentChanges.get(0).getText)
    )

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    didCodeChange(
      fileURI = new URI(params.getTextDocument.getUri),
      updatedCode = None
    )

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    didCodeChange(
      fileURI = new URI(params.getTextDocument.getUri),
      updatedCode = None
    )

  /**
   * [[PresentationCompiler]] reacts to all code changes tne same.
   *
   * @param fileURI     The file that changed.
   * @param updatedCode The code that changed in that file.
   */
  def didCodeChange(fileURI: URI,
                    updatedCode: Option[String]): Unit =
    this.synchronized { // TODO: Remove synchronized. Use async.
      val workspace = getOrInitWorkspace()

      val codeChangedState =
        PresentationCompiler.codeChanged(
          fileURI = fileURI,
          updatedCode = updatedCode,
          currentState = workspace
        )

      setState(state.updateWorkspace(codeChangedState))

      val compiledState =
        PresentationCompiler.parsedAndCompileWorkspace(codeChangedState)

      setState(state.updateWorkspace(compiledState))
    }

  override def completion(params: CompletionParams): CompletableFuture[messages.Either[util.List[CompletionItem], CompletionList]] =
    CompletableFutures.computeAsync {
      cancelChecker =>
        val fileURI = new URI(params.getTextDocument.getUri)
        val workspace = getOrInitWorkspace()

        val line = params.getPosition.getLine
        val character = params.getPosition.getCharacter

        cancelChecker.checkCanceled()

        val suggestions =
          PresentationCompiler.complete(
            line = line,
            character = character,
            uri = fileURI,
            workspace = workspace
          )

        val completionList =
          RalphLangClient.toCompletionList(suggestions)

        cancelChecker.checkCanceled()

        messages.Either.forRight[util.List[CompletionItem], CompletionList](completionList)
    }

  /**
   *
   * @param fileURI
   * @return
   */
  def getOrInitWorkspace(): WorkspaceState.Configured =
    this.synchronized { // TODO: Remove synchronized. Use async.
      val initialisedWorkspace =
        state.workspace match {
          case Some(workspace: WorkspaceState.Configured) =>
            workspace

          case Some(workspace: WorkspaceState.UnConfigured) =>
            PresentationCompiler.initialiseWorkspace(workspace) match {
              case Left(error) =>
                state.withClient {
                  implicit client =>
                    throw RalphLangClient.log(error)
                }

              case Right(workspace) =>
                workspace
            }

          case None =>
            // TODO: Check that this is reported to the client
            //        when a request is a not requiring a future
            throw RalphLangClient.responseError(
              errorCode = ResponseErrorCode.ServerNotInitialized,
              message = "Workspace folder not supplied"
            )
        }

      setState(state.updateWorkspace(initialisedWorkspace))

      initialisedWorkspace
    }

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    ()

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
    ()

  override def getTextDocumentService: TextDocumentService =
    this

  override def getWorkspaceService: WorkspaceService =
    this

  override def shutdown(): CompletableFuture[AnyRef] =
    CompletableFuture.completedFuture("TODO: shutdown")

  override def exit(): Unit =
    ()

}
