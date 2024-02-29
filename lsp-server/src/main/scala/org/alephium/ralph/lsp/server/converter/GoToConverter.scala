package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.access.compiler.message.CodeRange
import org.alephium.ralph.lsp.pc.gotodef.data.GoToLocation
import org.eclipse.lsp4j

/** Converts Go-to definition types to LSP4J types */
object GoToConverter {

  /** Convert [[GoToLocation]] to LSP4J type [[lsp4j.Location]] */
  def toLocation(goTo: GoToLocation): lsp4j.Location =
    new lsp4j.Location(
      goTo.uri.toString,
      toRange(goTo.codeRange)
    )

  /** Convert [[CodeRange]] to LSP4J type [[lsp4j.Range]] */
  def toRange(codeRange: CodeRange): lsp4j.Range =
    new lsp4j.Range(
      new lsp4j.Position(codeRange.from.line, codeRange.from.character),
      new lsp4j.Position(codeRange.to.line, codeRange.to.character)
    )

}
