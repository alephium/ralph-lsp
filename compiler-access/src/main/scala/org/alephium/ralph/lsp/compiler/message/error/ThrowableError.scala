package org.alephium.ralph.lsp.compiler.message.error

import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}

/**
 * Errors due to thrown exceptions.
 *
 * Stores the stacktrace for better error reporting in the IDE.
 */
case class ThrowableError(throwable: Throwable) extends CompilerMessage.Error {
  override def message: String =
    throwable.getMessage

  override def index: SourceIndex =
    SourceIndex.empty
}
