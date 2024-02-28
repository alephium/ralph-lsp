package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}

object ThrowableError {

  /** Without title */
  def apply(throwable: Throwable): ThrowableError =
    new ThrowableError(
      title = "",
      throwable = throwable
    )

}

/**
 * Errors due to thrown exceptions.
 *
 * Stores the stacktrace for future error report improvements.
 */
case class ThrowableError(title: String,
                          throwable: Throwable,
                          index: SourceIndex = SourceIndexExtra.zero) extends CompilerMessage.Error {
  override def message: String =
    if (title.isBlank)
      throwable.getMessage
    else
      title + ": " + throwable.getMessage
}
