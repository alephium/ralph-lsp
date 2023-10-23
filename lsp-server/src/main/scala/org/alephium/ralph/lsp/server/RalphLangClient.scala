package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.server.DataConverter._
import scala.collection.JavaConverters._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient

import java.net.URI
import java.util.ArrayList

/** Implements functions that are an extension to LSP4J's [[LanguageClient]]. */
object RalphLangClient {

  implicit class RalphLangClientExtension(val client: RalphLangClient) extends AnyVal {


    def setWatchers() ={
val watchers = new ArrayList[FileSystemWatcher](1)
val fsw = new FileSystemWatcher()
fsw.setGlobPattern("**/*.ral")
	watchers.add(fsw)
	val options = new DidChangeWatchedFilesRegistrationOptions(watchers)
client.registerCapability(new RegistrationParams(List(new Registration("WORKSPACE_WATCHED_FILES_ID",   "workspace/didChangeWatchedFiles", options)).asJava))
    }


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
                newWorkspace: Iterable[WorkspaceState.SourceAware]): Unit ={
      toPublishDiagnotics(currentWorkspace, newWorkspace) foreach {
        diagnostic =>
          // TODO: Isn't there a way in LSP to send all
          //       diagnotics to the client in a single request?
          client.publishDiagnostics(diagnostic)
      }
    }
  }
}

/**
 * The Ralph-LSP client.
 */
trait RalphLangClient extends LanguageClient
