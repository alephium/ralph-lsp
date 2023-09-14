package org.alephium.ralph.lsp.server

import org.eclipse.lsp4j.jsonrpc.messages.{ResponseErrorCode, ResponseError => LSP4JResponseError}
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException

sealed trait ResponseError extends LSP4JResponseError {
  def toException =
    new ResponseErrorException(this)
}

object ResponseError {
  case object ClientNotConfigured extends
    LSP4JResponseError(ResponseErrorCode.ServerNotInitialized, "Client not configured", null) with ResponseError

  case object WorkspaceFolderNotSupplied extends
    LSP4JResponseError(ResponseErrorCode.InvalidParams, "Root workspace folder not supplied", null) with ResponseError

  case object MultiWorkspaceFolderNotSupported extends
    LSP4JResponseError(ResponseErrorCode.InvalidParams, "Multiple root workspace folders are not supported", null) with ResponseError

}
