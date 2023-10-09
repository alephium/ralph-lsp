package org.alephium.ralph.lsp.compiler.message

import org.alephium.ralph.error.CompilerError.FormattableError

/**
 * Messages returned by compiler.
 *
 * In LSP this type is named Diagnostics.
 * */
sealed trait CompilerMessage {
  def message: String

  def index: SourceIndex
}

object CompilerMessage {

  /** Can be a formatted or unformatted error */
  sealed trait AnyError extends CompilerMessage

  /** Unformatted error type */
  trait Error extends AnyError

  /** Error message that can also output formatted error message to console */
  trait FormattedError extends AnyError {
    def error: FormattableError

    final def message: String =
      error.message

    final def index: SourceIndex =
      SourceIndex(
        index = error.position,
        width = error.foundLength
      )
  }

  /** Can be a formatted or unformatted warning */
  sealed trait AnyWarning extends CompilerMessage

  /** Unformatted error type */
  trait Warning extends AnyWarning
}
