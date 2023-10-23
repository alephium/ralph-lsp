package org.alephium.ralph.lsp.server.state

import org.alephium.ralph.lsp.pc.workspace.{WorkspaceChangeResult, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

object ServerStateUpdater {

  def workspaceChanged(change: WorkspaceChangeResult,
                       serverState: ServerState): Option[ServerState] =
    change match {
      case WorkspaceChangeResult.BuildChanged(buildChangeResult, cleanWorkspaceOnError) =>
        buildChangeResult match {
          case Some(buildResult) =>
            val newState =
              buildChanged(
                buildChangeResult = buildResult,
                cleanWorkspaceOnError = cleanWorkspaceOnError,
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
                           cleanWorkspaceOnError: Boolean,
                           serverState: ServerState): ServerState =
    buildChangeResult match {
      case Left(buildError) =>
        val newWorkspace =
          if (cleanWorkspaceOnError)
            serverState.workspace map {
              workspace =>
                WorkspaceState.Created(workspace.workspaceURI)
            }
          else
            serverState.workspace

        serverState.copy(
          buildErrors = Some(buildError),
          workspace = newWorkspace
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
