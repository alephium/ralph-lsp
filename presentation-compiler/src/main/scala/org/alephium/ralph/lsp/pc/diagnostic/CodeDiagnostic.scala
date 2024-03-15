package org.alephium.ralph.lsp.pc.diagnostic

import org.alephium.ralph.lsp.access.compiler.message.LineRange

case class CodeDiagnostic(range: LineRange, message: String, severity: CodeDiagnosticSeverity)
