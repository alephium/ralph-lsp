package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.RalphcCompiledConfig

import java.net.URI

object WorkspaceDeploy {

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
                           config: RalphcCompiledConfig)(implicit compiler: CompilerAccess): WorkspaceState.IsCompiled = {
    val result =
      compiler.compileForDeployment(
        workspaceURI = workspaceURI,
        config = config
      )

    WorkspaceStateBuilder.toWorkspaceState(
      currentState = ???,
      compilationResult = ???
    )
  }

}
