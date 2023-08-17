package org.alephium.ralph.lsp.server.service

import org.alephium.ralph.lsp.pc.completion.CodeCompleter
import org.alephium.ralph.lsp.pc.PresentationCompiler
import org.alephium.ralph.lsp.pc.data.DataConverter
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.services.{LanguageClient, TextDocumentService}

import java.util
import java.util.concurrent.CompletableFuture
import scala.concurrent.ExecutionContext
import scala.jdk.FutureConverters.FutureOps

/**
 * TODO: See if lsp4j allows a way for client not to be lazily initialised.
 *
 * @param client Lazily access client because an instance of this class
 *               required by lsp4j before a `client` instance is provided
 * @param ec     ExecutionContext used to serve client requests and notifications from server.
 */
class RalphTextDocumentService(client: => LanguageClient)(implicit ec: ExecutionContext) extends TextDocumentService {

  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    PresentationCompiler.compile(params.getTextDocument.getText) match {
      case Left(error) =>
        // report the compilation error
        val diagnostic =
          DataConverter.toPublishDiagnostics(
            uri = params.getTextDocument.getUri,
            code = params.getTextDocument.getText,
            error = error
          )

        client.publishDiagnostics(diagnostic)

      case Right(ast) =>
        // Good! Trigger a background process that builds semantic index for code-completion using this AST.
        CodeCompleter.buildSemanticIndex(params.getTextDocument.getUri, ast)
    }

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    ()

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    ()

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    ()

  override def completion(position: CompletionParams): CompletableFuture[messages.Either[util.List[CompletionItem], CompletionList]] =
    CodeCompleter
      .complete(position)
      .map(messages.Either.forRight[util.List[CompletionItem], CompletionList])
      .asJava
      .toCompletableFuture
}
