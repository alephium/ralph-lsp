// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.util

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error._

import java.net.URI

object TryUtil {

  /** Catch all exceptions thrown by `ralphc` and file-io */
  def catchAllThrows[T](fileURI: URI): PartialFunction[Throwable, Left[CompilerMessage.AnyError, T]] = {
    case throwable: Throwable =>
      Left(toCompilerMessage(fileURI, throwable))
  }

  /** Converts ralphc thrown exceptions to [[CompilerMessage.AnyError]] */
  def toCompilerMessage(
      fileURI: URI,
      throwable: Throwable): CompilerMessage.AnyError =
    throwable match {
      case error: FormattableError =>
        FormattedError(error)

      case error: Throwable =>
        ThrowableError(error, fileURI)
    }

}
