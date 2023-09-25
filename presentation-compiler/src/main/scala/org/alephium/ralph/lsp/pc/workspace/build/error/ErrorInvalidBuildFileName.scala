package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.pc.workspace.build.WorkspaceBuild

case class ErrorInvalidBuildFileName(fileName: String) extends FormattableError {
  override def title: String =
    "Error"

  override def message: String =
    s"Invalid build file name '$fileName'. Use '${WorkspaceBuild.BUILD_FILE_NAME}'."

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
