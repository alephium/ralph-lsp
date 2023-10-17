package org.alephium.ralph.lsp.access.util

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error._

object TryUtil {

  /** Catch all exceptions thrown by `ralphc` and file-io */
  def catchAllThrows[T]: PartialFunction[Throwable, Either[CompilerMessage.AnyError, T]] = {
    case error: FormattableError =>
      Left(FormattedError(error))

    case error: org.alephium.ralph.Compiler.Error =>
      Left(NativeError(error))

    case error: Throwable =>
      Left(ThrowableError(error))
  }

}
