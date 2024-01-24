package org.alephium.ralph.lsp.pc.diagnostics

sealed trait CodeDiagnosticSeverity

object CodeDiagnosticSeverity {
  case object Error extends CodeDiagnosticSeverity
  case object Warning extends CodeDiagnosticSeverity
}
