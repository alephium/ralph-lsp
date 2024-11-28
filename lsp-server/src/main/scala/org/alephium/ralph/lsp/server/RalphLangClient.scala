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

import org.alephium.ralph.lsp.utils.ExceptionUtil
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient

import java.util.concurrent.CompletableFuture

/**
 * The Ralph-LSP client.
 *
 * Implements functions accessing [[LanguageClient]] APIs.
 */
case class RalphLangClient(private val client: LanguageClient) {

  def show(error: ResponseError): ResponseError = {
    client.showMessage(new MessageParams(MessageType.Error, error.getMessage))
    error
  }

  def error(message: String): Unit =
    client.logMessage(new MessageParams(MessageType.Error, message))

  def error(
      message: String,
      cause: Throwable): Unit = {
    val clientMessage =
      ExceptionUtil.mergeToString(
        message = message,
        cause = cause
      )

    error(clientMessage)
  }

  def warning(message: String): Unit =
    client.logMessage(new MessageParams(MessageType.Warning, message))

  def info(message: String): Unit =
    client.logMessage(new MessageParams(MessageType.Info, message))

  def log(message: String): Unit =
    client.logMessage(new MessageParams(MessageType.Log, message))

  def trace(message: String): Unit =
    client.logTrace(new LogTraceParams(message))

  /**
   * @see [[RalphLangClient.registerCapability]]
   */
  def register(registration: Registration): CompletableFuture[Void] =
    client.registerCapability(new RegistrationParams(java.util.Arrays.asList(registration)))

  /**
   * @see [[RalphLangClient.publishDiagnostics]]
   */
  def publish(diagnostics: Iterable[PublishDiagnosticsParams]): Unit =
    diagnostics foreach client.publishDiagnostics

}
