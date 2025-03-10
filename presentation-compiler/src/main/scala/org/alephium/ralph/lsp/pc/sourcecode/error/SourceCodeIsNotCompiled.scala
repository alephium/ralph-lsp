// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}

import java.net.URI

case class SourceCodeIsNotCompiled(uri: URI) extends CompilerMessage.Error {

  override def message: String =
    s"Source code is on-disk and not compiled. URI: $uri"

  override def index: SourceIndex =
    SourceIndexExtra.zero(uri)

}
