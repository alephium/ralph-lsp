package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.server.RalphLangServer.serverCapabilities
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services._

import java.util
import java.util.concurrent.CompletableFuture
import scala.concurrent.ExecutionContext

object RalphLangServer {
  /** Build capabilities supported by the LSP server */
  def serverCapabilities(): ServerCapabilities = {
    val capabilities = new ServerCapabilities()

    capabilities.setCompletionProvider(new CompletionOptions(true, util.Arrays.asList(".")))
    //    capabilities.setWorkspaceSymbolProvider(true)
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)

    capabilities.setDiagnosticProvider(new DiagnosticRegistrationOptions(true, false))
    //    capabilities.setDocumentSymbolProvider(true)

    capabilities
  }
}

/**
 * The Ralph-LSP server.
 *
 * @param _client proxy for communicating with the client
 * @param ec      Globally used ExecutionContext
 */
class RalphLangServer(private var _client: Option[LanguageClient] = None)(implicit ec: ExecutionContext) extends LanguageServer with LanguageClientAware {

  private def client: LanguageClient =
    _client.getOrElse(throw new Exception("Client not initialised.")) // Nope! Do better than the suggested lsp4j way.

  override def connect(client: LanguageClient): Unit =
    _client = Some(client)

  // TODO: If allowed in this phase (maybe? the doc seem to indicate no), access the PresentationCompiler
  //       and do an initial workspace compilation.
  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    CompletableFuture.completedFuture(new InitializeResult(serverCapabilities()))


  override def shutdown(): CompletableFuture[AnyRef] =
    CompletableFuture.completedFuture("TODO: shutdown")

  override def exit(): Unit =
    ()

  override def getTextDocumentService: TextDocumentService = ???

  override def getWorkspaceService: WorkspaceService = ???
}
