// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.pc.diagnostic.{CodeDiagnosticSeverity, CodeDiagnostic, FileDiagnostic}
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

  private def toDiagnostic(diagnostic: CodeDiagnostic): Diagnostic =
    new Diagnostic(
      CommonConverter.toRange(diagnostic.range),
      diagnostic.message,
      toDiagnosticSeverity(diagnostic.severity),
      "RalphLSP"
    )

  private def toDiagnosticSeverity(severity: CodeDiagnosticSeverity): DiagnosticSeverity =
    severity match {
      case CodeDiagnosticSeverity.Error =>
        DiagnosticSeverity.Error

      case CodeDiagnosticSeverity.Warning =>
        DiagnosticSeverity.Warning
    }

}
