package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.pc.workspace.build.Build

case object ErrorBuildFileNotFound extends CompilerMessage.Error {
  override def message: String =
    s"Build file not found. Create a '${Build.BUILD_FILE_NAME}' file in the project's root folder."

  override def index: SourceIndex =
    SourceIndex.empty
}
