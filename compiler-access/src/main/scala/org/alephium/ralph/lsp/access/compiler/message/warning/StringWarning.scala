package org.alephium.ralph.lsp.access.compiler.message.warning

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}

import java.net.URI

/**
 * String warning reported by `ralphc` not containing source-location information.
 */
object StringWarning {
  @inline def apply(message: String, fileURI: URI): StringWarning =
    StringWarning(
      message = message,
      index = SourceIndexExtra.zero(fileURI)
    )
}

case class StringWarning(message: String, index: SourceIndex) extends CompilerMessage.Warning
