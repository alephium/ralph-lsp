package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.{Compiler, SourceIndex}
import org.alephium.ralph.error.CompilerError.FormattableError

object FileError {
  def apply(error: Compiler.Error): FileError =
    FileError(error.message)

  def apply(error: Throwable): FileError =
    FileError(error.getMessage)
}

/**
 * File level error.
 *
 * @param error error message
 */
case class FileError(error: String) extends FormattableError {
  override def title: String =
    s"Workspace error: $error"

  override def message: String =
    error

  override def index: SourceIndex =
    SourceIndex(0, 1)

}
