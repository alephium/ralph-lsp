package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}

import java.net.URI

case class ErrorInvalidBuildFileLocation(buildURI: URI, workspaceURI: URI) extends CompilerMessage.Error {
  override def message: String =
    "Build file must be placed in the root workspace directory"

  override def index: SourceIndex =
    SourceIndexExtra.zero(buildURI)
}
