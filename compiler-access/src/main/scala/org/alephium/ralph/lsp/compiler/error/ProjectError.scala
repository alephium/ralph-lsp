package org.alephium.ralph.lsp.compiler.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex

/**
 * Project level errors produced by `ralphc` that are not associated with a source file.
 */
case class ProjectError(override val message: String) extends FormattableError {
  override def title: String =
    "Project error"

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
