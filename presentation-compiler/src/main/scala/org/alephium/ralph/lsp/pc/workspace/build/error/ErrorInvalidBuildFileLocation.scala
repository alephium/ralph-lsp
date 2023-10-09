package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}

import java.net.URI

case class ErrorInvalidBuildFileLocation(buildURI: URI,
                                         workspaceURI: URI) extends CompilerMessage.Error {
  override def message: String =
    s"Build file '$buildURI' does not belong to workspace '$workspaceURI'. It must be placed in the root folder"

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
