package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

protected case class ServerState(client: Option[RalphLangClient] = None,
                                 workspace: Option[WorkspaceState] = None) {

  def withClient[T](f: RalphLangClient => T): T =
    client match {
      case Some(client) =>
        f(client)

      case None =>
        throw ResponseError.ClientNotConfigured.toResponseErrorException
    }

  def updateWorkspace(workspace: WorkspaceState): ServerState =
    this.copy(workspace = Some(workspace))
}
