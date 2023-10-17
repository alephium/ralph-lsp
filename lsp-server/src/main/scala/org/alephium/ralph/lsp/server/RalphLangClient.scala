package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.server.DataConverter._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient

import java.net.URI

object RalphLangClient {

  /** Implements functions that are an extension to LSP4J's [[LanguageClient]]. */
  implicit class RalphLangClientExtension(val client: RalphLangClient) extends AnyVal {

    def log(error: ResponseError): ResponseError = {
      client.logMessage(new MessageParams(MessageType.Error, error.getMessage))
      error
    }

    def publishErrors(fileURI: URI,
                      code: Option[String],
                      errors: List[CompilerMessage.AnyError]): Unit = {
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
                newWorkspace: Option[WorkspaceState.SourceAware]): Unit =
      toPublishDiagnotics(
        previousState = currentWorkspace,
        nextState = newWorkspace
      ) foreach {
        diagnostic =>
          client.publishDiagnostics(diagnostic)
      }
  }
}

/**
 * The Ralph-LSP client.
 */
trait RalphLangClient extends LanguageClient
