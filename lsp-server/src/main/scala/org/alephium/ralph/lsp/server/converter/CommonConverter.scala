// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.access.compiler.message.{LinePosition, LineRange}
import org.eclipse.lsp4j

/** Implements converts for common types shared between all LSP4J types */
object CommonConverter {

  def toRange(range: LineRange): lsp4j.Range =
    new lsp4j.Range(
      toPosition(range.from),
      toPosition(range.to)
    )

  @inline def toPosition(position: LinePosition): lsp4j.Position =
    new lsp4j.Position(position.line, position.character)

}
