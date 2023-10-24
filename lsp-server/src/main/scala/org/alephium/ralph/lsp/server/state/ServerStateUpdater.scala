package org.alephium.ralph.lsp.server.state

import org.alephium.ralph.lsp.pc.workspace.{WorkspaceChangeResult, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

object ServerStateUpdater {

  /**
   * Given the workspace change [[WorkspaceChangeResult]]
   * and current [[ServerState]] return a new [[ServerState]]
   * */
  def workspaceChanged(change: WorkspaceChangeResult,
                       serverState: ServerState): Option[ServerState] =
    change match {
      case WorkspaceChangeResult.BuildChanged(buildChangeResult) =>
        buildChangeResult map {
          buildResult =>
            buildChanged(
              buildChangeResult = buildResult,
              serverState = serverState
            )
        }

      case WorkspaceChangeResult.SourceChanged(sourceChangeResult) =>
        val newState =
          sourceCodeChanged(
            sourceChangeResult = sourceChangeResult,
            serverState = serverState
          )

        Some(newState)
    }

  /** Apply build change to the [[ServerState]] */
  private def buildChanged(buildChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
                           serverState: ServerState): ServerState =
    buildChangeResult match {
      case Left(buildError) =>
        // fetch the activateWorkspace to replace existing workspace or-else
        // use continue with existing workspace
        val newWorkspace =
          buildError.activateWorkspace orElse serverState.workspace

        serverState.copy(
          buildErrors = Some(buildError),
          workspace = newWorkspace
        )

      case Right(newWorkspace) =>
        // build errors got resolved, clear buildErrors.
        serverState.copy(
          buildErrors = None,
          workspace = Some(newWorkspace)
        )
    }

  /** Apply source-code change to the [[ServerState]] */
  private def sourceCodeChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
                                serverState: ServerState): ServerState =
    sourceChangeResult match {
      case Left(buildError) =>
        serverState.copy(buildErrors = Some(buildError))

      case Right(newWorkspace) =>
        serverState.copy(workspace = Some(newWorkspace))
    }
}
