package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}

object StringError {
  @inline def apply(message: String): StringError =
    StringError(
      message = message,
      index = SourceIndex.empty
    )
}
/**
 * String error reported by `ralphc` not containing source-location information.
 */
case class StringError(message: String,
                       index: SourceIndex) extends CompilerMessage.Error
