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
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.eclipse.lsp4j
import org.eclipse.lsp4j.TextEdit

import java.net.URI

/** Converts Go-to definition types to LSP4J types */
object GoToConverter {

  /** Convert [[SourceLocation.GoTo]]s to LSP4J types [[lsp4j.Location]] */
  def toLocations(goTos: Iterator[SourceLocation.GoTo]): Iterator[lsp4j.Location] =
    goTos flatMap toLocation

  /** Converts rename results to LSP4J type [[lsp4j.TextEdit]] */
  def toTextEdits(
      goTos: Iterator[SourceLocation.Rename],
      newText: String): Map[URI, List[lsp4j.TextEdit]] =
    goTos
      .flatMap {
        goTo =>
          toTextEdit(
            goTo = goTo,
            newText = newText
          )
      }
      .toList
      .groupMap(_._1)(_._2)

  /** Convert [[SourceLocation.GoTo]] to LSP4J type [[lsp4j.Location]] */
  private def toLocation(goTo: SourceLocation.GoTo): Option[lsp4j.Location] =
    goTo.toLineRange() map {
      lineRange =>
        new lsp4j.Location(
          goTo.parsed.fileURI.toString,
          toRange(lineRange)
        )
    }

  /** Convert [[LineRange]] to LSP4J type [[lsp4j.Range]] */
  private def toRange(range: LineRange): lsp4j.Range =
    new lsp4j.Range(
      new lsp4j.Position(range.from.line, range.from.character),
      new lsp4j.Position(range.to.line, range.to.character)
    )

  /** Converts a rename result to LSP4J type [[lsp4j.TextEdit]] */
  private def toTextEdit(
      goTo: SourceLocation.Rename,
      newText: String): Option[(URI, TextEdit)] =
    goTo.toLineRange() map {
      lineRange =>
        val range    = toRange(lineRange)
        val textEdit = new lsp4j.TextEdit(range, newText)
        val fileURI  = goTo.parsed.fileURI
        (fileURI, textEdit)
    }

}
