package org.alephium.ralph.lsp.compiler.message

import org.alephium.ralph.error.CompilerError.FormattableError

sealed trait CompilerMessage {
  def message: String

  def index: SourceIndex
}

object CompilerMessage {

  sealed trait AnyError extends CompilerMessage

  trait Error extends AnyError

  trait FormattedError extends AnyError {
    def error: FormattableError

    def message: String =
      error.message

    def index: SourceIndex =
      SourceIndex(
        index = error.position,
        width = error.foundLength
      )
  }

  sealed trait AnyWarning extends CompilerMessage
  trait Warning extends AnyWarning
}
