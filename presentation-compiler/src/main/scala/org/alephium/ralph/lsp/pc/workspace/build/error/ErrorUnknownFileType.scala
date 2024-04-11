package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}

import java.net.URI

case class ErrorUnknownFileType(fileURI: URI) extends CompilerMessage.Error {

  override def message: String =
    s"Unknown file type: $fileURI"

  override def index: SourceIndex =
    SourceIndexExtra.zero(fileURI)

}
