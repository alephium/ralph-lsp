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

import org.alephium.ralph.lsp.access.compiler.message.LineRange
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.eclipse.lsp4j

/** Converts Go-to definition types to LSP4J types */
object GoToConverter {

  /** Convert [[GoToLocation]]s to LSP4J types [[lsp4j.Location]] */
  def toLocations(goTos: Iterator[GoToLocation]): Iterator[lsp4j.Location] =
    goTos map toLocation

  /** Convert [[GoToLocation]] to LSP4J type [[lsp4j.Location]] */
  def toLocation(goTo: GoToLocation): lsp4j.Location =
    new lsp4j.Location(
      goTo.uri.toString,
      toRange(goTo.lineRange)
    )

  /** Convert [[LineRange]] to LSP4J type [[lsp4j.Range]] */
  def toRange(range: LineRange): lsp4j.Range =
    new lsp4j.Range(
      new lsp4j.Position(range.from.line, range.from.character),
      new lsp4j.Position(range.to.line, range.to.character)
    )

}
