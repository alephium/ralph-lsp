package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}

import scala.collection.immutable.ArraySeq

protected case class ServerState(client: Option[RalphLangClient] = None,
                                 workspaces: ArraySeq[WorkspaceState] = ArraySeq.empty) {

  def updateWorkspace(newState: WorkspaceState.Configured): ServerState = {
    val index = workspaces.indexWhere(_.workspaceURI == newState.workspaceURI)

    val newStates =
      if (index >= 0)
        workspaces.updated(index, newState)
      else
        workspaces appended newState

    this.copy(workspaces = newStates)
  }

  def withClient[T](f: RalphLangClient => T): T =
    client match {
      case Some(client) =>
        f(client)

      case None =>
        val error = new ResponseError(ResponseErrorCode.ServerNotInitialized, "Client not configured", null)
        val exception = new ResponseErrorException(error)
        // TODO: log this error
        throw exception
    }
}
