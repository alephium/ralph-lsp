package org.alephium.ralph.lsp.compiler.message.error

import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}

/**
 * String error reported by `ralphc` not containing source-location information.
 */

object StringError {
  @inline def apply(throwable: Throwable): StringError =
    StringError(
      message = throwable.getMessage,
      index = SourceIndex.empty
    )

  @inline def apply(message: String): StringError =
    StringError(
      message = message,
      index = SourceIndex.empty
    )
}

case class StringError(message: String,
                       index: SourceIndex) extends CompilerMessage.Error
