package org.alephium.ralph.lsp.pc.data

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex

case class WorkspaceError(exception: Throwable) extends FormattableError {
  override def title: String =
    exception.getMessage

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
