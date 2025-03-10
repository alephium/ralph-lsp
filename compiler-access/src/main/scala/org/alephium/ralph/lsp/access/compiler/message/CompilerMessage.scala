// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.message

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.error.CompilerError.FormattableError

/**
 * A compiler message can be an `error` or a `warning`.
 *
 * In LSP this type is named Diagnostics.
 */
sealed trait CompilerMessage {

  def message: String

  def index: SourceIndex

}

object CompilerMessage {

  /** A formatted or unformatted error */
  sealed trait AnyError extends CompilerMessage

  /** Unformatted error type */
  trait Error extends AnyError

  /** Error that can also output formatted message to console */
  trait FormattedError extends AnyError {

    def error: FormattableError

    final def message: String =
      error.message

    final def index: SourceIndex =
      SourceIndex(
        index = error.position,
        width = error.foundLength,
        fileURI = error.fileURI
      )

  }

  /** A formatted or unformatted warning */
  sealed trait AnyWarning extends CompilerMessage

  /** Unformatted warning type */
  trait Warning extends AnyWarning

}
