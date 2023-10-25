package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}

/**
 * Stores error type [[org.alephium.ralph.Compiler.Error]] which is commonly used by ralph compiler.
 *
 * [[org.alephium.ralph.Compiler.Error]] also contains a stacktrace
 * which could be used to provided better error reports to the user.
 */
case class NativeError(error: org.alephium.ralph.Compiler.Error) extends CompilerMessage.Error {
  override def message: String =
    error.getMessage

  override def index: SourceIndex =
    SourceIndex.empty
}
