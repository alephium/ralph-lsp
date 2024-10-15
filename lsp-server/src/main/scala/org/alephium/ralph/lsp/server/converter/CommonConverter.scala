// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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

  @inline private def toPosition(position: LinePosition): lsp4j.Position =
    new lsp4j.Position(position.line, position.character)

}
