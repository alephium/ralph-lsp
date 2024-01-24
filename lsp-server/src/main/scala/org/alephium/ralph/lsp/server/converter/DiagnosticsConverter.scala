package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.pc.diagnostics.{CodeDiagnostic, CodeDiagnosticSeverity, FileDiagnostic}
import org.eclipse.lsp4j._
import scala.jdk.CollectionConverters.SeqHasAsJava

/** Convert diagnostics returned from presentation-compiler to LSP4J types */
object DiagnosticsConverter {

  def toPublishParams(diagnostics: Iterable[FileDiagnostic]): Iterable[PublishDiagnosticsParams] =
    diagnostics map toPublishParams

  def toPublishParams(fileDiagnostic: FileDiagnostic): PublishDiagnosticsParams = {
    val diagnostics = fileDiagnostic.codeDiagnostics map toDiagnostic
    new PublishDiagnosticsParams(fileDiagnostic.fileURI.toString, diagnostics.asJava)
  }

  def toDiagnostic(diagnostic: CodeDiagnostic): Diagnostic =
    new Diagnostic(
      new Range(
        new Position(diagnostic.fromLine, diagnostic.fromCharacter),
        new Position(diagnostic.toLine, diagnostic.toCharacter)
      ),
      diagnostic.message,
      toDiagnosticSeverity(diagnostic.severity),
      "RalphLSP"
    )

  def toDiagnosticSeverity(severity: CodeDiagnosticSeverity): DiagnosticSeverity =
    severity match {
      case CodeDiagnosticSeverity.Error =>
        DiagnosticSeverity.Error

      case CodeDiagnosticSeverity.Warning =>
        DiagnosticSeverity.Warning
    }

}
