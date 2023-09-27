package org.alephium.ralph.lsp.server

import org.eclipse.lsp4j.jsonrpc.messages.{ResponseErrorCode, ResponseError => LSP4JResponseError}
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException

import java.net.URI

sealed abstract class ResponseError(errorCode: ResponseErrorCode,
                                    message: String) extends LSP4JResponseError(errorCode, message, null) {
  def toResponseErrorException =
    new ResponseErrorException(this)
}

object ResponseError {
  case object ClientNotConfigured extends
    ResponseError(
      errorCode = ResponseErrorCode.ServerNotInitialized,
      message = "Client not configured"
    )

  case object WorkspaceFolderNotSupplied extends
    ResponseError(
      errorCode = ResponseErrorCode.InvalidParams,
      message = "Root workspace folder not supplied"
    )

  case object UnableToInitialiseWorkspace extends
    ResponseError(
      errorCode = ResponseErrorCode.InternalError,
      message = "Unable to initialise workspace"
    )

  case object MultiRootWorkspaceFoldersNotSupported extends
    ResponseError(
      errorCode = ResponseErrorCode.InvalidParams,
      message = "Multiple root workspace folders are not supported"
    )

  case class UnknownFileType(fileURI: URI) extends
    ResponseError(
      errorCode = ResponseErrorCode.InvalidRequest,
      message = s"Unknown file type: $fileURI"
    )

}
