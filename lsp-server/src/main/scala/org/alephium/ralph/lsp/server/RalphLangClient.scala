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

  def publishErrors(fileURI: URI,
                    code: Option[String],
                    errors: List[FormattableError])(implicit client: RalphLangClient): Unit = {
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
                   error: CompilerError.FormattableError,
                   severity: DiagnosticSeverity): Diagnostic = {
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

    new Diagnostic(range, error.message, severity, "RalphLS")
  }

  def toWorkspaceDiagnostics(workspace: WorkspaceState.Configured): PublishDiagnosticsParams = {
    val workspaceDiagnostics =
      workspace match {
        case compiled: WorkspaceState.Errored =>
          compiled.workspaceErrors map {
            error =>
              // These are workspace level errors such as `Compiler.Error`, their source-code information is unknown.
              toDiagnostic(
                code = None,
                error = error,
                severity = DiagnosticSeverity.Error
              )
          }

        case _ =>
          Seq.empty
      }

    new PublishDiagnosticsParams(workspace.workspaceURI.toString, workspaceDiagnostics.asJava)
  }

  def toSourceCodeDiagnostics(state: WorkspaceState.Configured): Iterable[PublishDiagnosticsParams] =
    state.sourceCode collect {
      case state: SourceCodeState.ErrorSource =>
        // transform multiple source code errors to diagnostics.
        val diagnostics =
          state.errors map {
            error =>
              toDiagnostic(
                code = Some(state.code),
                error = error,
                severity = DiagnosticSeverity.Error
              )
          }

        new PublishDiagnosticsParams(state.fileURI.toString, diagnostics.asJava)

      case state: SourceCodeState.ErrorAccess =>
        // transform single source code access error to diagnostics.
        val diagnostics =
          toDiagnostic(
            code = None,
            error = state.error,
            severity = DiagnosticSeverity.Error
          )

        new PublishDiagnosticsParams(state.fileURI.toString, util.Arrays.asList(diagnostics))

      case state: SourceCodeState.Compiled =>
        // transform source code warning messages to diagnostics.
        val diagnostics =
          state.warnings map {
            warning =>
              toDiagnostic(
                code = Some(state.code),
                error = warning,
                severity = DiagnosticSeverity.Warning
              )
          }

        new PublishDiagnosticsParams(state.fileURI.toString, diagnostics.asJava)
    }

  def toPublishDiagnostics(workspace: WorkspaceState.Configured): Iterable[PublishDiagnosticsParams] = {
    val sourceCodeDiagnostics = toSourceCodeDiagnostics(workspace)
    val workspaceDiagnostics = toWorkspaceDiagnostics(workspace)
    sourceCodeDiagnostics ++ Seq(workspaceDiagnostics)
  }

  def toPublishDiagnostics(fileURI: URI,
                           code: Option[String],
                           errors: List[FormattableError],
                           severity: DiagnosticSeverity): PublishDiagnosticsParams = {
    val diagnostics =
      errors map {
        error =>
          toDiagnostic(
            code = code,
            error = error,
            severity = severity
          )
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
