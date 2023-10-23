package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.workspace.build.BuildState

sealed trait WorkspaceChangeResult
object WorkspaceChangeResult {

  /**
   * Build change result
   *
   * @param buildChangeResult     Compilation outcome
   * @param cleanWorkspaceOnError If true, starts a fresh workspace, else continues with existing workspace.
   */
  case class BuildChanged(buildChangeResult: Option[Either[BuildState.BuildErrored, WorkspaceState.SourceAware]],
                          cleanWorkspaceOnError: Boolean) extends WorkspaceChangeResult

  /**
   * Source-code change result
   *
   * @param sourceChangeResult Compilation outcome
   */
  case class SourceChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState.SourceAware]) extends WorkspaceChangeResult
}
