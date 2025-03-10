// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.util

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error._

import java.net.URI

object TryUtil {

  /** Catch all exceptions thrown by `ralphc` and file-io */
  def catchAllThrows[T](fileURI: URI): PartialFunction[Throwable, Either[CompilerMessage.AnyError, T]] = {
    case error: FormattableError =>
      Left(FormattedError(error))

    case error: Throwable =>
      Left(ThrowableError(error, fileURI))
  }

}
