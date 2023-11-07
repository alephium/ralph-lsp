package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.server.MessageMethods._

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.messages
import scala.jdk.CollectionConverters.SeqHasAsJava
import java.util.concurrent.CompletableFuture

object RalphLangClient {

  /** Implements functions that are an extension to LSP4J's [[LanguageClient]]. */
  implicit class RalphLangClientExtension(val client: RalphLangClient) extends AnyVal {

    def log(error: ResponseError): ResponseError = {
      client.logMessage(new MessageParams(MessageType.Error, error.getMessage))
      error
    }

    def registerWatchedFiles(): CompletableFuture[Void] = {
      val watchers = Seq(new FileSystemWatcher(messages.Either.forLeft("**/*"))).asJava;
      val options = new DidChangeWatchedFilesRegistrationOptions(watchers);
      val registration = new Registration(WORKSPACE_WATCHED_FILES_ID, WORKSPACE_WATCHED_FILES, options)

      client.registerCapability(new RegistrationParams(Seq(registration).asJava))
    }
  }
}

/**
 * The Ralph-LSP client.
 */
trait RalphLangClient extends LanguageClient
