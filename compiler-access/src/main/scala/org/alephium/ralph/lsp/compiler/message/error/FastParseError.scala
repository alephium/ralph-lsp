package org.alephium.ralph.lsp.compiler.message.error

import fastparse.Parsed
import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.lsp.compiler.message.CompilerMessage

object FastParseError {

  @inline def apply(failure: Parsed.Failure): FastParseError =
    FastParseError(CompilerError.FastParseError(failure))

}

case class FastParseError(error: CompilerError.FastParseError) extends CompilerMessage.FormattedError
