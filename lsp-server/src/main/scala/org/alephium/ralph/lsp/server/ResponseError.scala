// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server

import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{ResponseErrorCode, ResponseError => LSP4JResponseError}

import java.net.URI

sealed abstract class ResponseError(
    errorCode: ResponseErrorCode,
    message: String)
  extends LSP4JResponseError(errorCode, message, null) {

  def toResponseErrorException =
    new ResponseErrorException(this)

}

object ResponseError {

  case object ClientNotConfigured
    extends ResponseError(
      errorCode = ResponseErrorCode.ServerNotInitialized,
      message = "Client not configured"
    )

  case object WorkspaceFolderNotSupplied
    extends ResponseError(
      errorCode = ResponseErrorCode.InvalidParams,
      message = "Root workspace folder not supplied"
    )

  case class SourceNotInWorkspace(fileURI: URI)
    extends ResponseError(
      errorCode = ResponseErrorCode.InvalidRequest,
      message = s"Source file not within an active workspace: '$fileURI'"
    )

  case class UnknownFileType(fileURI: URI)
    extends ResponseError(
      errorCode = ResponseErrorCode.InvalidRequest,
      message = s"Unknown file type: $fileURI"
    )

  case class InvalidTraceSetting(setting: String)
    extends ResponseError(
      errorCode = ResponseErrorCode.InvalidRequest,
      message = s"Invalid trace setting: $setting"
    )

  case object ShutdownRequested
    extends ResponseError(
      errorCode = ResponseErrorCode.InvalidRequest,
      message = "Request Ignored: A server shutdown request is in progress"
    )

  /** An unexpected internal error */
  case class InternalError(exception: Throwable)
    extends ResponseError(
      errorCode = ResponseErrorCode.InternalError,
      message = exception.toString
    )

  case class StringInternalError(message: String)
    extends ResponseError(
      errorCode = ResponseErrorCode.InternalError,
      message = message
    )

  case object WorkspaceNotCompiled
    extends ResponseError(
      errorCode = ResponseErrorCode.InternalError,
      message = "Workspace is not compiled"
    )

}
