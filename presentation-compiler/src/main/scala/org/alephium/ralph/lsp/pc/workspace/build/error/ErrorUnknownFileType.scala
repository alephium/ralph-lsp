package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.pc.util.URIUtil

import java.net.URI

case class ErrorUnknownFileType(fileURI: URI) extends FormattableError {
  override def title: String =
    "Error"

  override def message: String =
    s"Unknown file extension '${URIUtil.getFileName(fileURI)}'"

  override def index: SourceIndex =
    SourceIndex(0, 0)
}
