package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.server.MessageMethods._

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.messages
import java.util.concurrent.CompletableFuture

object RalphLangClient {

  /** Implements functions that are an extension to LSP4J's [[LanguageClient]]. */
  implicit class RalphLangClientExtension(val client: RalphLangClient) extends AnyVal {

    def log(error: ResponseError): ResponseError = {
      client.logMessage(new MessageParams(MessageType.Error, error.getMessage))
      error
    }

    def registerWatchedFiles(): CompletableFuture[Void] = {
      val watchers = java.util.Arrays.asList(new FileSystemWatcher(messages.Either.forLeft("**/*")))
      val options = new DidChangeWatchedFilesRegistrationOptions(watchers)
      val registration = new Registration(WORKSPACE_WATCHED_FILES_ID, WORKSPACE_WATCHED_FILES, options)

      client.registerCapability(new RegistrationParams(java.util.Arrays.asList(registration)))
    }
  }
}

/**
 * The Ralph-LSP client.
 */
trait RalphLangClient extends LanguageClient
