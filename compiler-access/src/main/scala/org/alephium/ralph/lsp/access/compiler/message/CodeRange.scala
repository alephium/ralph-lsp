package org.alephium.ralph.lsp.access.compiler.message

object CodeRange {
  val zero: CodeRange =
    CodeRange(
      from = CodePosition.zero,
      to = CodePosition.zero
    )
}

case class CodeRange(from: CodePosition,
                     to: CodePosition)
