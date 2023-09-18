package org.alephium.ralph.lsp.pc

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.compiler.error.StringError
import org.alephium.ralph.lsp.pc.completion.{CodeCompleter, Suggestion}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceBuild, WorkspaceState}
import org.alephium.ralphc.Config

import java.net.URI

/**
 * Implements the public API to be used for interactive programming in Ralph.
 *
 * [[PresentationCompiler]] is immutable.
 *
 * Depends on `ralphc` for compilation and all source-code IO.
 */
object PresentationCompiler {

  def buildChanged(fileURI: URI, build: Option[String]): Either[FormattableError, WorkspaceState.Built] =
    WorkspaceBuild.buildChanged(
      fileURI = fileURI,
      build = build
    ) map WorkspaceState.Built

  def createWorkspace(workspaceURI: URI): WorkspaceState.Initialised =
    WorkspaceState.Initialised(workspaceURI)

  /**
   * Initial workspaces collects paths to all OnDisk ralph files.
   *
   * @param config compiler configuration file.
   * @return
   */

  def initialiseWorkspace(workspace: WorkspaceState.Built)(implicit compiler: CompilerAccess): Either[CompilerError.FormattableError, WorkspaceState.UnCompiled] =
    Workspace.initialise(workspace)

  /**
   * Parses and compiles the workspaces.
   *
   * Note: Parsing is executed lazily. If the code is already parsed, it will not be re-parsed and only be re-compiled.
   *
   * @param workspace Current workspace state
   * @return New workspace state
   */
  def parseAndCompileWorkspace(workspace: WorkspaceState.Configured)(implicit compiler: CompilerAccess): WorkspaceState.Configured =
    Workspace.parseAndCompile(workspace)

  /**
   * Compile the code in preparation for deployment. The final step in compilation.
   *
   * The state of presentation compiler is not used here. All ralph code files are accessed from disk.
   * LSP client should ensure that all files are flushed to disk.
   *
   * @param workspaceURI Workspace path
   * @param config       Compiler configuration
   * @param compiler     Target ralph compiler
   * @return New workspace state that PresentationalCompiler can continue with.
   */
  def compileForDeployment(workspaceURI: URI,
                           config: Config)(implicit compiler: CompilerAccess): WorkspaceState =
    Workspace.compileForDeployment(
      workspaceURI = workspaceURI,
      config = config
    )

  /**
   * Apply the code changes to the workspace state.
   *
   * @param fileURI      File updated
   * @param updatedCode  Updated code
   * @param currentState Current workspace state
   * @return New workspace state.
   */
  def codeChanged(fileURI: URI,
                  updatedCode: Option[String],
                  currentState: WorkspaceState.Configured): WorkspaceState.UnCompiled = {
    val newSourceCodeState =
      updatedCode match {
        case Some(newCode) =>
          SourceCodeState.UnCompiled(fileURI, newCode)

        case None =>
          SourceCodeState.OnDisk(fileURI)
      }

    val updatedFileStates =
      currentState.updateOrAdd(newSourceCodeState)

    WorkspaceState.UnCompiled(
      build = currentState.build,
      sourceCode = updatedFileStates
    )
  }

  /**
   * Fetches existing workspace or initialises a new one from the configured build file.
   *
   * If the build file is not configured, it returns an error.
   */
  def getOrInitWorkspace(workspace: WorkspaceState)(implicit compiler: CompilerAccess): Either[FormattableError, WorkspaceState.Configured] =
    workspace match {
      case workspace: WorkspaceState.Configured =>
        // Workspace is fully configured. Return it!
        Right(workspace)

      case workspace: WorkspaceState.Built =>
        // Build is configured. Initialise the workspace!
        initialiseWorkspace(workspace)

      case _: WorkspaceState.Initialised =>
        // Build file is not supplied. Report missing build file.
        Left(StringError(WorkspaceBuild.buildNotFound()))
    }

  /**
   * Execute code completing given the current workspace state.
   */
  def complete(line: Int,
               character: Int,
               uri: URI,
               workspace: WorkspaceState): Array[Suggestion] =
    CodeCompleter.complete(
      line = line,
      character = character,
      uri = uri,
      workspace = workspace
    )

}
