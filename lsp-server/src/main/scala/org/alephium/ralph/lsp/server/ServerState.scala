package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.util.concurrent.{Future => JFuture}

protected case class ServerState(client: Option[RalphLangClient],
                                 listener: Option[JFuture[Void]],
                                 workspace: Option[WorkspaceState]) {

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
