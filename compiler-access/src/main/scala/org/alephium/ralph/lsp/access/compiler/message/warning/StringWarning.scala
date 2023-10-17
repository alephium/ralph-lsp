package org.alephium.ralph.lsp.access.compiler.message.warning

import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}

/**
 * String warning reported by `ralphc` not containing source-location information.
 */
object StringWarning {
  @inline def apply(message: String): StringWarning =
    StringWarning(
      message = message,
      index = SourceIndex.empty
    )
}

case class StringWarning(message: String,
                         index: SourceIndex) extends CompilerMessage.Warning
