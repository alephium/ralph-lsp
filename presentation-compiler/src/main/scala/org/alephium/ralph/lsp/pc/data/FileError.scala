package org.alephium.ralph.lsp.pc.data

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.{Compiler, SourceIndex}

object FileError {
  def apply(error: Compiler.Error): FileError = {
    scribe.error(error)
    FileError(error.message)
  }

  def apply(error: Throwable): FileError = {
    scribe.error(error)
    FileError(error.getMessage)
  }
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
