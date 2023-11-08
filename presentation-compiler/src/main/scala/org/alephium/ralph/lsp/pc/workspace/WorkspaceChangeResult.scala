package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.workspace.build.BuildState

sealed trait WorkspaceChangeResult
object WorkspaceChangeResult {

  /**
   * Build change result
   *
   * @param buildChangeResult Build compilation outcome
   */
  case class BuildChanged(buildChangeResult: Option[Either[BuildState.BuildErrored, WorkspaceState.IsSourceAware]]) extends WorkspaceChangeResult

  /**
   * Source-code change result
   *
   * @param sourceChangeResult Source compilation outcome
   */
  case class SourceChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState.IsSourceAware]) extends WorkspaceChangeResult
}
