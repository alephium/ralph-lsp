package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}

import scala.collection.immutable.ArraySeq

protected case class ServerState(client: Option[RalphLangClient] = None,
                                 workspaceStates: Option[ArraySeq[WorkspaceState]] = None) {

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
