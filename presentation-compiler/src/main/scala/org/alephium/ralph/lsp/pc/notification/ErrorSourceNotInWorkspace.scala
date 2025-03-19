// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.notification

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.SourceIndex

import java.net.URI

case class ErrorSourceNotInWorkspace(fileURI: URI) extends CompilerMessage.Error {

  override def message: String =
    s"Source file not within an active workspace: '$fileURI'"

  override def index: SourceIndex =
    SourceIndexExtra.zero(fileURI)

}
