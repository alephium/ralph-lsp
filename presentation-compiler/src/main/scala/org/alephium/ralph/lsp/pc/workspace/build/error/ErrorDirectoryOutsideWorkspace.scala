package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}

case class ErrorDirectoryOutsideWorkspace(dirPath: String,
                                          index: SourceIndex) extends CompilerMessage.Error {
  override def message: String =
    s"Directory '$dirPath' is not within the current workspace"
}
