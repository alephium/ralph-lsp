package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

case class ErrorDirectoryDoesNotExists(dirPath: String,
                                       index: SourceIndex) extends CompilerMessage.Error {
  override def message: String =
    s"Directory '$dirPath' does not exist"
}
