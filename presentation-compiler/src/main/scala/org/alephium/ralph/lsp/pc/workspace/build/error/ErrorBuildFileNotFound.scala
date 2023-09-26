package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.pc.workspace.build.WorkspaceBuild

case object ErrorBuildFileNotFound extends FormattableError {
  override def title: String =
    "Error"

  override def message: String =
    s"Build file not found. Create a '${WorkspaceBuild.BUILD_FILE_NAME}' file in the project's root folder."

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
