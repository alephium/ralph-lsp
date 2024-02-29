package org.alephium.ralph.lsp.access.compiler.message

object CodePosition {
  val zero: CodePosition =
    CodePosition(line = 0, character = 0)
}

case class CodePosition(line: Int, character: Int)
