// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.notification

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.SourceIndex

object ErrorWorkspaceFolderNotSupplied extends CompilerMessage.Error {

  override def message: String =
    "Root workspace folder not supplied"

  override def index: SourceIndex =
    SourceIndex.empty

}
