package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex

case class FileWarning(warning: String) extends FormattableError {
  override def title: String =
    s"Workspace warning: $warning"

  override def message: String =
    warning

  override def index: SourceIndex =
    SourceIndex(0, 1)
}
