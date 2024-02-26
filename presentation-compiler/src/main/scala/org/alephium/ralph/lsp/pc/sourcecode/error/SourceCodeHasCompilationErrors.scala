package org.alephium.ralph.lsp.pc.sourcecode.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.SourceIndex

import java.net.URI

case class SourceCodeHasCompilationErrors(uri: URI) extends CompilerMessage.Error {
  override def message: String =
    s"Source code has compilation error(s). URI: $uri"

  override def index: SourceIndex =
    SourceIndexExtra.zero
}
