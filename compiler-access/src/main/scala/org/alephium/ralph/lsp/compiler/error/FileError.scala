package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.{Compiler, SourceIndex}
import org.alephium.ralph.error.CompilerError.FormattableError

object FileError {
  def apply(error: Compiler.Error): FileError =
    FileError(error.message)
}

/**
 * File level error.
 */
case class FileError(override val message: String) extends FormattableError {
  override def title: String =
    "File error"

  override def index: SourceIndex =
    SourceIndex(0, 1)

}
