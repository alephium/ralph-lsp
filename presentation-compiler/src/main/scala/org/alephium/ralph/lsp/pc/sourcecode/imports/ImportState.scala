package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.alephium.ralph.Ast.ContractWithState

sealed trait ImportState

object ImportState {

  case class Parsed(name: ImportName, index: Int, fullParse: String, fullParseIndex: Int) extends ImportState

  case class Compiled(parsed: Parsed, compiledCode: Seq[ContractWithState]) extends ImportState
}
