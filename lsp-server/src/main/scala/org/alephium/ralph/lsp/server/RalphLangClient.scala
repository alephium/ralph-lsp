package org.alephium.ralph.lsp.server

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient

import java.util.concurrent.CompletableFuture

/**
 * The Ralph-LSP client.
 */
case class RalphLangClient(private val client: LanguageClient) {

  def show(error: ResponseError): ResponseError = {
    client.showMessage(new MessageParams(MessageType.Error, error.getMessage))
    error
  }

  def error(message: String): Unit =
    client.logMessage(new MessageParams(MessageType.Error, message))

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
