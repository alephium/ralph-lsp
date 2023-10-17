package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}

/**
 * Errors due to thrown exceptions.
 *
 * Stores the stacktrace for future error report improvements.
 */
case class ThrowableError(throwable: Throwable) extends CompilerMessage.Error {
  override def message: String =
    throwable.getMessage

  override def index: SourceIndex =
    SourceIndex.empty
}
