package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}

import java.net.URI

object StringError {
  @inline def apply(message: String, fileURI: URI): StringError =
    StringError(
      message = message,
      index = SourceIndexExtra.zero(fileURI)
    )
}

/**
 * String error reported by `ralphc` not containing source-location information.
 */
case class StringError(message: String, index: SourceIndex) extends CompilerMessage.Error
