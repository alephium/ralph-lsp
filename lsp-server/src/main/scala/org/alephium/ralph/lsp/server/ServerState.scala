package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.config.IDEConfig
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseError, ResponseErrorCode}

import java.util.concurrent.atomic.AtomicInteger

object ServerState {
  private val version: AtomicInteger =
    new AtomicInteger(0)
}

protected case class ServerState(client: Option[RalphLangClient] = None,
                                 ideConfig: Option[IDEConfig] = None,
                                 workspaceState: Option[WorkspaceState] = None) {

  val version: Int =
    ServerState.version.incrementAndGet()

  def withClient[T](f: RalphLangClient => T): T =
    client match {
      case Some(client) =>
        f(client)

      case None =>
        val error = new ResponseError(ResponseErrorCode.ServerNotInitialized, "Client not configured", null)
        val exception = new ResponseErrorException(error)
        throw exception
    }

  def withIDEConfig[T](f: IDEConfig => T): T =
    ideConfig match {
      case Some(ideConfig) =>
        f(ideConfig)

      case None =>
        withClient {
          implicit client =>
            val error = new ResponseError(ResponseErrorCode.InvalidParams, "IDEConfig not configured", null)
            val exception = new ResponseErrorException(error)
            RalphLangClient.log(exception)
            throw exception
        }
    }
}
