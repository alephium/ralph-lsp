package org.alephium.ralph.lsp.pc.sourcecode.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}

import java.net.URI

case class SourceCodeAccessFailed(uri: URI) extends CompilerMessage.Error {
  override def message: String =
    s"Source code errored on access. URI: $uri"

  override def index: SourceIndex =
    SourceIndex.zero
}
