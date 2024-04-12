// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.access.compiler.message.{LinePosition, LineRange}
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

  def toDiagnostic(diagnostic: CodeDiagnostic): Diagnostic =
    new Diagnostic(
      toRange(diagnostic.range),
      diagnostic.message,
      toDiagnosticSeverity(diagnostic.severity),
      "RalphLSP"
    )

  def toRange(range: LineRange): Range =
    new Range(
      toPosition(range.from),
      toPosition(range.to)
    )

  def toPosition(position: LinePosition): Position =
    new Position(position.line, position.character)

  def toDiagnosticSeverity(severity: CodeDiagnosticSeverity): DiagnosticSeverity =
    severity match {
      case CodeDiagnosticSeverity.Error =>
        DiagnosticSeverity.Error

      case CodeDiagnosticSeverity.Warning =>
        DiagnosticSeverity.Warning
    }

}
