package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.completion.CodeCompleter
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceBuild, WorkspaceState}
import org.alephium.ralph.lsp.server.RalphLangServer._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.{messages, CompletableFutures, ResponseErrorException}
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
            throw ResponseError.WorkspaceFolderNotSupplied.toResponseErrorException
          else if (workspaceFolders.size > 1)
            throw ResponseError.MultiRootWorkspaceFoldersNotSupported.toResponseErrorException
          else
            new URI(workspaceFolders.head.getUri)

        val workspace =
          Workspace.create(workspaceURI)

        setState(state.updateWorkspace(workspace))

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
   * [[Workspace]] reacts to all code/build changes tne same.
   *
   * @param fileURI File that changed.
   * @param code    Content of the file.
   */

  def didCodeChange(fileURI: URI,
                    code: Option[String]): Unit = {
    val fileExtension = URIUtil.getFileExtension(fileURI)
    if (fileExtension == WorkspaceBuild.FILE_EXTENSION)
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
      state.withClient {
        implicit client =>
          throw
            RalphLangClient
              .log(ResponseError.UnknownFile(fileURI))
              .toResponseErrorException
      }
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

      if (fileName == WorkspaceBuild.FILE_NAME)
        Workspace.build(
          buildURI = fileURI,
          build = code,
          state = getWorkspace()
        ) match {
          case Left(error) =>
            state.withClient {
              implicit client =>
                RalphLangClient.publishErrors(
                  fileURI = fileURI,
                  code = code, // TODO: the code here needs to be from the read build file.
                  errors = List(error)
                )
            }

          case Right(newState) =>
            state.withClient {
              implicit client =>
                // drop existing state and start with a new build file.
                setState(state.updateWorkspace(newState))
                client.refreshDiagnostics() // request project wide re-build TODO: Handle future
            }
        }
      else
        state.withClient {
          implicit client =>
            throw
              RalphLangClient
                .log(ResponseError.InvalidBuildFileName(fileName))
                .toResponseErrorException
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

      Workspace.codeChanged(
        fileURI = fileURI,
        updatedCode = updatedCode,
        currentState = workspace
      ) match {
        case Left(error) =>
          state.withClient {
            implicit client =>
              RalphLangClient.publishErrors(
                fileURI = fileURI,
                code = updatedCode, // TODO: Test if this code is the updated code.
                errors = List(error)
              )
          }

        case Right(newState) =>
          // immediately set the new state with the updated code so it is available for other threads.
          setState(state.updateWorkspace(newState))

          // compile the new state.
          val compiledState =
            Workspace.compile(newState)

          // set the compiled state
          setState(state.updateWorkspace(compiledState))
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
          RalphLangClient.toCompletionList(suggestions)

        cancelChecker.checkCanceled()

        messages.Either.forRight[util.List[CompletionItem], CompletionList](completionList)
    }

  /**
   * @return Existing workspace or initialises a new one from the configured build file.
   *         Or else reports any workspace issues.
   */
  def getOrInitWorkspace(): WorkspaceState.Configured =
    this.synchronized { // TODO: Remove synchronized. Use async.
      state.workspace match {
        case Some(workspace) =>
          Workspace.getOrInit(workspace) match {
            case Left(error) =>
              throw
                state.withClient {
                  implicit client =>
                    RalphLangClient.log(error)
                }

            case Right(state) =>
              state
          }

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
    state.withClient {
      implicit client =>
        RalphLangClient
          .log(ResponseError.WorkspaceFolderNotSupplied)
          .toResponseErrorException
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
