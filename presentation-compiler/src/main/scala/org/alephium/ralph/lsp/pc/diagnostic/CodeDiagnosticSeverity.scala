package org.alephium.ralph.lsp.pc.diagnostic

sealed trait CodeDiagnosticSeverity

/**
 * TODO: This type should be removed and [[CodeDiagnostic]] should provide Error and Warning types
 *       This temporarily exists because [[CodeDiagnostic]] was moved from `lsp-server`'s LSP4J version
 *       with minimal changes and LSP4J has the type `DiagnosticSeverity`.
 */
object CodeDiagnosticSeverity {
  case object Error extends CodeDiagnosticSeverity
  case object Warning extends CodeDiagnosticSeverity
}
