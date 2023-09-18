package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.error.CompilerError.FormattableError

/**
 * An error or warning that reports a `String` error messages
 * reported by `ralphc` not containing source-location information.
 */
case class StringMessage(override val message: String) extends FormattableError {
  override def title: String =
    "Error"

  override def index: SourceIndex =
    SourceIndex(0, 1)

}
