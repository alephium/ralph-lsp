package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex

/**
 * Workspace level error
 */
case class WorkspaceError(override val message: String) extends FormattableError {
  override def title: String =
    "Workspace error"

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
