// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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

  case object MultiRootWorkspaceFoldersNotSupported
    extends ResponseError(
      errorCode = ResponseErrorCode.InvalidParams,
      message = "Multiple root workspace folders are not supported"
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
