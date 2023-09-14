package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex

/**
 * Workspace level error
 */
case class WorkspaceError(exception: Throwable) extends FormattableError {
  override def title: String =
    exception.getMessage

  override def message: String =
    exception.getMessage

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
