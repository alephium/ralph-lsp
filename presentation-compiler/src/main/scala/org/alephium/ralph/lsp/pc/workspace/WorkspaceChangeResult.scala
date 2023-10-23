package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.workspace.build.BuildState

sealed trait WorkspaceChangeResult
object WorkspaceChangeResult {
  case class BuildChanged(buildChangeResult: Option[Either[BuildState.BuildErrored, WorkspaceState.SourceAware]]) extends WorkspaceChangeResult
  case class SourceChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState.SourceAware]) extends WorkspaceChangeResult
}
