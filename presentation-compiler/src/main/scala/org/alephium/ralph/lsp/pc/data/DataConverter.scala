package org.alephium.ralph.lsp.pc.data

import org.alephium.ralph.error.CompilerError
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, Position, PublishDiagnosticsParams, Range}

import java.util

/**
 * Converts data types between lsp4j & Ralph.
 */
object DataConverter {

  /** Convert Ralph's FormattableError to lsp4j's Diagnostic */
  def toDiagnostics(code: String, error: CompilerError.FormattableError): Diagnostic = {
    val formatter = error.toFormatter(code)

    val start = new Position(formatter.sourcePosition.rowIndex, formatter.sourcePosition.colIndex)
    val end = new Position(formatter.sourcePosition.rowIndex, formatter.sourcePosition.colIndex + formatter.foundLength)
    val range = new Range(start, end)

    val message = error.message
    new Diagnostic(range, message, DiagnosticSeverity.Error, "TODO: source")
  }

  /** Convert Ralph's FormattableError to lsp4j's publishable diagnostic
   *
   * TODO: CompilerError should already have uri information.
   * */
  def toPublishDiagnostics(uri: String, code: String, error: CompilerError.FormattableError): PublishDiagnosticsParams = {
    val diagnostic = toDiagnostics(code, error)
    new PublishDiagnosticsParams(uri, util.Arrays.asList(diagnostic))
  }

}
