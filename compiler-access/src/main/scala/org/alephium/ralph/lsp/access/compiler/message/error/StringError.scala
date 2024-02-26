package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.SourceIndex

object StringError {
  @inline def apply(message: String): StringError =
    StringError(
      message = message,
      index = SourceIndexExtra.zero
    )
}
/**
 * String error reported by `ralphc` not containing source-location information.
 */
case class StringError(message: String,
                       index: SourceIndex) extends CompilerMessage.Error
