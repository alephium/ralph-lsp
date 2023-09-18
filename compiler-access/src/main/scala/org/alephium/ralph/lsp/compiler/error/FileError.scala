package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.error.CompilerError.FormattableError

/**
 * File level error that do not have errored source-location information.
 */
case class FileError(override val message: String) extends FormattableError {
  override def title: String =
    "File error"

  override def index: SourceIndex =
    SourceIndex(0, 1)

}
