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
