package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.workspace.build.BuildState

sealed trait WorkspaceChangeResult
object WorkspaceChangeResult {

  /**
   * Build change result
   *
   * @param buildChangeResult Compilation outcome
   */
  case class BuildChanged(buildChangeResult: Option[Either[BuildState.BuildErrored, WorkspaceState.SourceAware]]) extends WorkspaceChangeResult

  /**
   * Source-code change result
   *
   * @param sourceChangeResult Compilation outcome
   */
  case class SourceChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState.SourceAware]) extends WorkspaceChangeResult
}
