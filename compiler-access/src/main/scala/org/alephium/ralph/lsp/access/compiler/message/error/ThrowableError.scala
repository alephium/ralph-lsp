package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}

import java.net.URI

object ThrowableError {

  /** Without title */
  def apply(throwable: Throwable,
            fileURI: URI): ThrowableError =
    new ThrowableError(
      title = "",
      throwable = throwable,
      index = SourceIndexExtra.zero(fileURI)
    )

}

/**
 * Errors due to thrown exceptions.
 *
 * Stores the stacktrace for future error report improvements.
 */
case class ThrowableError(title: String,
                          throwable: Throwable,
                          index: SourceIndex) extends CompilerMessage.Error {
  override def message: String =
    if (title.isBlank)
      throwable.getMessage
    else
      title + ": " + throwable.getMessage
}
