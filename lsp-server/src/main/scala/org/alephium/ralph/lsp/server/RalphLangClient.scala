package org.alephium.ralph.lsp.server

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient

object RalphLangClient {

  /** Implements functions that are an extension to LSP4J's [[LanguageClient]]. */
  implicit class RalphLangClientExtension(val client: RalphLangClient) extends AnyVal {

    def log(error: ResponseError): ResponseError = {
      client.logMessage(new MessageParams(MessageType.Error, error.getMessage))
      error
    }

  }
}

/**
 * The Ralph-LSP client.
 */
trait RalphLangClient extends LanguageClient
