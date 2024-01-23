package org.alephium.ralph.lsp.server.state

import org.alephium.ralph.lsp.pc.state.PCState
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceChangeResult, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

object ServerStateUpdater {

  /**
   * Given the workspace change [[WorkspaceChangeResult]]
   * and current [[ServerState]] return a new [[ServerState]]
   * */
  def workspaceChanged(change: WorkspaceChangeResult,
                       pcState: PCState): Option[PCState] =
    change match {
      case WorkspaceChangeResult.BuildChanged(buildChangeResult) =>
        buildChangeResult map {
          buildResult =>
            buildChanged(
              buildChangeResult = buildResult,
              pcState = pcState
            )
        }

      case WorkspaceChangeResult.SourceChanged(sourceChangeResult) =>
        val newState =
          sourceCodeChanged(
            sourceChangeResult = sourceChangeResult,
            serverState = pcState
          )

        Some(newState)
    }

  /** Apply build change to the [[ServerState]] */
  private def buildChanged(buildChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
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

  /** Apply source-code change to the [[ServerState]] */
  private def sourceCodeChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
                                serverState: PCState): PCState =
    sourceChangeResult match {
      case Left(buildError) =>
        serverState.copy(buildErrors = Some(buildError))

      case Right(newWorkspace) =>
        serverState.copy(workspace = newWorkspace)
    }
}
