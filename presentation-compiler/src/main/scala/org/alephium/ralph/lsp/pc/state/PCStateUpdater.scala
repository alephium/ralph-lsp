package org.alephium.ralph.lsp.pc.state

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

object PCStateUpdater {

  /** Apply build change to the [[PCState]] */
  def buildChanged(buildChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
                   pcState: PCState): PCState =
    buildChangeResult match {
      case Left(buildError) =>
        // fetch the activateWorkspace to replace existing workspace
        // or-else continue with existing workspace
        val newWorkspace =
          buildError.activateWorkspace getOrElse pcState.workspace

        pcState.copy(
          buildErrors = Some(buildError),
          workspace = newWorkspace
        )

      case Right(newWorkspace) =>
        // build errors got resolved, clear it from state.
        pcState.copy(
          buildErrors = None,
          workspace = newWorkspace
        )
    }

  /** Apply source-code change to the [[PCState]] */
  def sourceCodeChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
                        pcState: PCState): PCState =
    sourceChangeResult match {
      case Left(buildError) =>
        pcState.copy(buildErrors = Some(buildError))

      case Right(newWorkspace) =>
        pcState.copy(workspace = newWorkspace)
    }
}
