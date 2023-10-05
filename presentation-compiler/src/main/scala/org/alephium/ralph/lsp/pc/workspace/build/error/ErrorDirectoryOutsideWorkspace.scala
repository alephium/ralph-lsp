package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex

case class ErrorDirectoryOutsideWorkspace(dirPath: String,
                                          index: SourceIndex) extends FormattableError {
  override def title: String =
    "Error"

  override def message: String =
    s"Directory '$dirPath' is not within the current workspace"
}
