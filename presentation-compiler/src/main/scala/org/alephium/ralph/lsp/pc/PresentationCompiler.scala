package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.pc.completion.{CodeCompleter, Suggestion}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}
import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralphc.Config

import java.net.URI
import scala.util.Try

/**
 * Implements the public API to be used for interactive programming in Ralph.
 *
 * PresentationCompiler is immutable.
 *
 * Internally [[PresentationCompiler]] depends on Ralph's core compiler.
 */
object PresentationCompiler {

  /**
   * Initial workspaces collects paths to all OnDisk ralph files.
   *
   * @param config compiler configuration file.
   * @return
   */
  def initialiseWorkspace(config: Config)(implicit compiler: CompilerAccess): Try[WorkspaceState.UnCompiled] =
    Workspace.initialise(config)

  /**
   * Parses and compiles the workspaces.
   *
   * Note: Parsing is executed lazily. If the code is already parsed, it will not be re-parsed and only be re-compiled.
   *
   * @param state           current workspace state
   * @param compilerOptions Ralph core compiler configuration
   * @param compiler        Target ralph compiler
   * @return new workspace state
   */
  def parsedAndCompileWorkspace(state: WorkspaceState.UnCompiled,
                                compilerOptions: CompilerOptions)(implicit compiler: CompilerAccess): WorkspaceState =
    Workspace.parseAndCompile(
      wsState = state,
      compilerOptions = compilerOptions
    )

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
                           config: Config)(implicit compiler: CompilerAccess): WorkspaceState = {
    Workspace.compileForDeployment(
      workspaceURI = workspaceURI,
      config = config
    )
  }

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
                  currentState: WorkspaceState): WorkspaceState.UnCompiled = {
    val newSourceCodeState =
      updatedCode match {
        case Some(newCode) =>
          SourceCodeState.UnCompiled(fileURI, newCode)

        case None =>
          SourceCodeState.OnDisk(fileURI)
      }

    val updatedFileStates =
      currentState.updateOrAdd(newSourceCodeState)

    WorkspaceState.UnCompiled(updatedFileStates)
  }

  /**
   * Execute code completing given the current workspace state.
   */
  def complete(line: Int,
               character: Int,
               uri: URI,
               state: WorkspaceState): Array[Suggestion] =
    CodeCompleter.complete(
      line = line,
      character = character,
      uri = uri,
      workspace = state
    )

}
