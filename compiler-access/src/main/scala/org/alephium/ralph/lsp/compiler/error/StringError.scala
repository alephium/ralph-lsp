package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.error.CompilerError.FormattableError

/**
 * String error reported by `ralphc` not containing source-location information.
 */
case class StringError(override val message: String) extends FormattableError {
  override def title: String =
    "Warning"

  override def index: SourceIndex =
    SourceIndex(0, 1)

}
