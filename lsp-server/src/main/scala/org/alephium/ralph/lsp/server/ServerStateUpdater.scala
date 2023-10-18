package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.workspace.{WorkspaceChangeResult, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

object ServerStateUpdater {

  def workspaceChanged(change: WorkspaceChangeResult,
                       serverState: ServerState): Option[ServerState] =
    change match {
      case WorkspaceChangeResult.BuildChanged(buildChangeResult) =>
        buildChangeResult match {
          case Some(buildResult) =>
            val newState =
              buildChanged(
                buildChangeResult = buildResult,
                serverState = serverState
              )

            Some(newState)

          case None =>
            None
        }

      case WorkspaceChangeResult.SourceChanged(sourceChangeResult) =>
        val newState =
          sourceCodeChanged(
            sourceChangeResult = sourceChangeResult,
            serverState = serverState
          )

        Some(newState)
    }

  /** Publish build file change result */
  private def buildChanged(buildChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
                           serverState: ServerState): ServerState =
    buildChangeResult match {
      case Left(buildError) =>
        serverState.copy(
          buildErrors = Some(buildError)
        )

      case Right(newWorkspace) =>
        serverState.copy(
          buildErrors = None,
          workspace = Some(newWorkspace)
        )
    }

  /** Publish source-code change result */
  private def sourceCodeChanged(sourceChangeResult: Either[BuildState.BuildErrored, WorkspaceState],
                                serverState: ServerState): ServerState =
    sourceChangeResult match {
      case Left(buildError) =>
        serverState.copy(buildErrors = Some(buildError))

      case Right(newWorkspace) =>
        serverState.copy(workspace = Some(newWorkspace))
    }
}
