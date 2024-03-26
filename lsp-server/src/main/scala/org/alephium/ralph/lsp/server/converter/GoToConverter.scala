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
