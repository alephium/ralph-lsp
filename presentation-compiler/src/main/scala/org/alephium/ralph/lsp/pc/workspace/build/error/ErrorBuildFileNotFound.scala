package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.pc.workspace.build.WorkspaceBuild

case object ErrorBuildFileNotFound extends CompilerMessage.Error {
  override def message: String =
    s"Build file not found. Create a '${WorkspaceBuild.BUILD_FILE_NAME}' file in the project's root folder."

  override def index: SourceIndex =
    SourceIndex.empty
}
