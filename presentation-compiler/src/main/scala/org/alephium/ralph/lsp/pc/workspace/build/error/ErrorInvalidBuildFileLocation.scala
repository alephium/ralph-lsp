package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex

import java.net.URI

case class ErrorInvalidBuildFileLocation(buildURI: URI,
                                         workspaceURI: URI) extends FormattableError {
  override def title: String =
    "Error"

  override def message: String =
    s"Build file '$buildURI' does not belong to workspace '$workspaceURI'. It must be placed in the root folder"

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
