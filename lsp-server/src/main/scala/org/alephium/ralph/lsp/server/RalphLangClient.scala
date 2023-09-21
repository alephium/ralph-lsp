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

    /** Report error at file level */
    def publish(workspace: WorkspaceState.SourceAware): Unit =
      toPublishDiagnostics(workspace) foreach {
        diagnostic =>
          // TODO: Isn't there a way in LSP to send all
          //       diagnotics to the client in a single request?
          client.publishDiagnostics(diagnostic)
      }

    def publish(workspaces: Iterable[WorkspaceState]): Unit =
      workspaces foreach {
        case _: WorkspaceState.Initialised | _: WorkspaceState.BuildCompiled =>
          ()

        case workspace: WorkspaceState.SourceAware =>
          publish(workspace)
      }
  }
}

/**
 * The Ralph-LSP client.
 */
trait RalphLangClient extends LanguageClient
