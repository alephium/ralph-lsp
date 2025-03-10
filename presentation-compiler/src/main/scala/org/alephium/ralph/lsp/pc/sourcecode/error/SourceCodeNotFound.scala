// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}

import java.net.URI

case class SourceCodeNotFound(uri: URI) extends CompilerMessage.Error {

  override def message: String =
    s"Source code not found. URI: $uri"

  override def index: SourceIndex =
    SourceIndexExtra.zero(uri)

}
