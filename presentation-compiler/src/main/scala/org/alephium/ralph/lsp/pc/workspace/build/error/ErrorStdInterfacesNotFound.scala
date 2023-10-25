package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.pc.workspace.build.Build

case object ErrorStdInterfacesNotFound extends CompilerMessage.Error {
  override def message: String =
    s"Std interfaces can't be found, can't build the project"

  override def index: SourceIndex =
    SourceIndex.empty
}
