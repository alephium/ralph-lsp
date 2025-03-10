// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.eclipse.lsp4j

/** Converts Go-to definition types to LSP4J types */
object GoToConverter {

  /** Convert [[SourceLocation.GoTo]]s to LSP4J types [[lsp4j.Location]] */
  def toLocations(goTos: Iterator[SourceLocation.GoTo]): Iterator[lsp4j.Location] =
    goTos flatMap toLocation

  /** Convert [[SourceLocation.GoTo]] to LSP4J type [[lsp4j.Location]] */
  private def toLocation(goTo: SourceLocation.GoTo): Option[lsp4j.Location] =
    goTo.toLineRange() map {
      lineRange =>
        new lsp4j.Location(
          goTo.parsed.fileURI.toString,
          CommonConverter.toRange(lineRange)
        )
    }

}
