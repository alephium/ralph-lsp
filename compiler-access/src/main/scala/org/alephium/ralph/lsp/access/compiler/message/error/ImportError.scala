// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

sealed trait ImportError extends CompilerMessage.Error

object ImportError {

  final case class Unknown(ast: Tree.Import) extends ImportError {

    val message: String = s"Unknown import: ${ast.string.value}"

    override def index: SourceIndex =
      ast.string.index

  }

}
