// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.CollectionUtil
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{Location, LocationLink}
import org.eclipse.lsp4j.jsonrpc.messages

import java.util

/** Converts Go-to definition types to LSP4J types */
object GoToConverter {

  /** Convert [[SourceLocation.GoTo]]s to LSP4J's either type [[lsp4j.Location]] */
  def toLocationEither(locations: Iterable[SourceLocation.GoTo]): messages.Either[util.List[_ <: Location], util.List[_ <: LocationLink]] =
    messages.Either.forLeft[util.List[_ <: Location], util.List[_ <: LocationLink]](toLocations(locations))

  def toLocations(locations: Iterable[SourceLocation.GoTo]): util.ArrayList[Location] = {
    val javaLocations =
      GoToConverter.toLocations(locations.iterator)

    CollectionUtil.toJavaList(javaLocations)
  }

  /** Convert [[SourceLocation.GoTo]]s to LSP4J types [[lsp4j.Location]] */
  private def toLocations(goTos: Iterator[SourceLocation.GoTo]): Iterator[lsp4j.Location] =
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
