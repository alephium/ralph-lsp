package org.alephium.ralph.lsp.server

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.pc.completion.Suggestion
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.LanguageClient

import java.net.URI
import java.util
import scala.jdk.CollectionConverters.SeqHasAsJava

object RalphLangClient {

  /** **********
   * Client API
   * *********** */

  /** Report error at project level */
  def log(error: FormattableError)(implicit client: RalphLangClient): FormattableError = {
    client.logMessage(new MessageParams(MessageType.Error, error.message))
    error
  }

  def log(error: ResponseError)(implicit client: RalphLangClient): ResponseError = {
    client.logMessage(new MessageParams(MessageType.Error, error.getMessage))
    error
  }

  def publish(fileURI: URI,
              code: Option[String],
              errors: List[FormattableError])(implicit client: RalphLangClient): Unit = {
    val publish = toPublishDiagnostics(fileURI, code, errors)
    client.publishDiagnostics(publish)
  }

  /** Report error at file level */
  def publish(workspace: WorkspaceState.Configured)(implicit client: RalphLangClient): Unit =
    toPublishDiagnostics(workspace) foreach {
      diagnostic =>
        // TODO: Isn't there a way in LSP to send all
        //       diagnotics to the client in a single request?
        client.publishDiagnostics(diagnostic)
    }

  def publish(workspaces: Iterable[WorkspaceState])(implicit client: RalphLangClient): Unit =
    workspaces foreach {
      case _: WorkspaceState.Initialised | _: WorkspaceState.Built =>
        ()

      case workspace: WorkspaceState.Configured =>
        RalphLangClient.publish(workspace)
    }

  /** **************
   * Data converters
   * *************** */

  /** Convert Ralph's FormattableError to lsp4j's Diagnostic */
  def toDiagnostic(code: Option[String],
                   error: CompilerError.FormattableError): Diagnostic = {
    val range =
      code match {
        case Some(code) =>
          val formatter = error.toFormatter(code)

          val start = new Position(formatter.sourcePosition.rowIndex, formatter.sourcePosition.colIndex)
          val end = new Position(formatter.sourcePosition.rowIndex, formatter.sourcePosition.colIndex + formatter.sourcePosition.width)
          new Range(start, end)

        case None =>
          new Range(new Position(0, 0), new Position(0, 1))
      }

    new Diagnostic(range, error.message, DiagnosticSeverity.Error, "RalphLS")
  }

  def toWorkspaceDiagnostics(workspace: WorkspaceState.Configured): PublishDiagnosticsParams = {
    val workspaceDiagnostics =
      workspace match {
        case compiled: WorkspaceState.Compiled =>
          compiled.workspaceErrors map {
            error =>
              toDiagnostic(
                code = None,
                error = error
              )
          }

        case _ =>
          Seq.empty
      }

    new PublishDiagnosticsParams(workspace.build.workspaceURI.toString, workspaceDiagnostics.asJava)
  }

  def toSourceCodeDiagnostics(state: WorkspaceState.Configured): Iterable[PublishDiagnosticsParams] =
    state.sourceCode collect {
      case state: SourceCodeState.ErrorSource =>
        val diagnostics =
          state.errors map {
            error =>
              toDiagnostic(
                code = Some(state.code),
                error = error
              )
          }

        new PublishDiagnosticsParams(state.fileURI.toString, diagnostics.asJava)
    }

  def toPublishDiagnostics(workspace: WorkspaceState.Configured): Iterable[PublishDiagnosticsParams] = {
    val sourceCodeErrors = toSourceCodeDiagnostics(workspace)
    val workspaceErrors = toWorkspaceDiagnostics(workspace)
    sourceCodeErrors ++ Seq(workspaceErrors)
  }

  def toPublishDiagnostics(fileURI: URI,
                           code: Option[String],
                           errors: List[FormattableError]): PublishDiagnosticsParams = {
    val diagnostics =
      errors map {
        error =>
          toDiagnostic(code, error)
      }

    new PublishDiagnosticsParams(fileURI.toString, diagnostics.asJava)
  }

  def toCompletionList(suggestions: Array[Suggestion]): CompletionList = {
    val items = new util.ArrayList[CompletionItem]()

    suggestions foreach {
      suggestion =>
        val item = new CompletionItem()
        item.setLabel(suggestion.label)
        item.setDetail(suggestion.detail)
        item.setDocumentation(suggestion.documentation)
        item.setInsertText(suggestion.insert)
        item.setKind(CompletionItemKind.valueOf(suggestion.productPrefix))
        items.add(item)
    }

    new CompletionList(items)
  }

}

trait RalphLangClient extends LanguageClient
