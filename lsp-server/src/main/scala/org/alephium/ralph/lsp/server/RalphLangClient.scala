package org.alephium.ralph.lsp.server

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient

import java.util.concurrent.CompletableFuture

object RalphLangClient {

  /** Implements functions that are an extension to LSP4J's [[LanguageClient]]. */
  implicit class RalphLangClientExtension(val client: RalphLangClient) extends AnyVal {

    def show(error: ResponseError): ResponseError = {
      client.showMessage(new MessageParams(MessageType.Error, error.getMessage))
      error
    }

    /**
     * @see [[RalphLangClient.registerCapability]]
     */
    def register(registration: Registration): CompletableFuture[Void] =
      client.registerCapability(new RegistrationParams(java.util.Arrays.asList(registration)))
  }
}

/**
 * The Ralph-LSP client.
 */
trait RalphLangClient extends LanguageClient
