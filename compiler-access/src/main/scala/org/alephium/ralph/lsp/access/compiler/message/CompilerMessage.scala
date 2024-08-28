// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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
