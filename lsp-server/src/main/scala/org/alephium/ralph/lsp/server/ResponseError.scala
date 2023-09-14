package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.workspace.WorkspaceBuild
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

  case object MultiRootWorkspaceFoldersNotSupported extends
    ResponseError(
      errorCode = ResponseErrorCode.InvalidParams,
      message = "Multiple root workspace folders are not supported"
    )

  case class UnknownFile(fileURI: URI) extends
    ResponseError(
      errorCode = ResponseErrorCode.InvalidRequest,
      message = s"Unknown file '${fileURI.getPath}'"
    )

  case class InvalidBuildFileName(name: String) extends
    ResponseError(
      errorCode = ResponseErrorCode.InvalidParams,
      message = s"Invalid build file name '$name'. Use '${WorkspaceBuild.FILE_NAME}'"
    )

}
