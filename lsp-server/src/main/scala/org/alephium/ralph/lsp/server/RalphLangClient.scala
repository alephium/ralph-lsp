package org.alephium.ralph.lsp.server

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.server.DataConverter._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient

import java.net.URI

/** Implements functions that are an extension to LSP4J's [[LanguageClient]]. */
object RalphLangClient {

  implicit class RalphLangClientExtension(val client: RalphLangClient) extends AnyVal {

    /** Report error at project level */
    def log(error: FormattableError): FormattableError = {
      client.logMessage(new MessageParams(MessageType.Error, error.message))
      error
    }

    def log(error: ResponseError): ResponseError = {
      client.logMessage(new MessageParams(MessageType.Error, error.getMessage))
      error
    }

    def publishErrors(fileURI: URI,
                      code: Option[String],
                      errors: List[FormattableError]): Unit = {
      val publish =
        toPublishDiagnostics(
          fileURI = fileURI,
          code = code,
          errors = errors,
          severity = DiagnosticSeverity.Error
        )

      client.publishDiagnostics(publish)
    }

    /** Publish IDE messages given the workspace previous and newer states */
    def publish(currentWorkspace: WorkspaceState.SourceAware,
                newWorkspace: Iterable[WorkspaceState.SourceAware]): Unit =
      toPublishDiagnotics(currentWorkspace, newWorkspace) foreach {
        diagnostic =>
          // TODO: Isn't there a way in LSP to send all
          //       diagnotics to the client in a single request?
          client.publishDiagnostics(diagnostic)
      }
  }
}

/**
 * The Ralph-LSP client.
 */
trait RalphLangClient extends LanguageClient
