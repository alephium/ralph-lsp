package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.util.concurrent.{Future => JFuture}

protected case class ServerState(client: Option[RalphLangClient],
                                 listener: Option[JFuture[Void]],
                                 workspace: Option[WorkspaceState]) {

  def updateWorkspace(workspace: WorkspaceState): ServerState =
    this.copy(workspace = Some(workspace))
}
