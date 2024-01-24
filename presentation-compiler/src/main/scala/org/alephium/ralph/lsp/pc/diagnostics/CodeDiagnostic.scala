package org.alephium.ralph.lsp.pc.diagnostics

case class CodeDiagnostic(fromLine: Int,
                          fromCharacter: Int,
                          toLine: Int,
                          toCharacter: Int,
                          message: String,
                          severity: CodeDiagnosticSeverity)
