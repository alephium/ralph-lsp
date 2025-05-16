// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.CollectionUtil
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{InlayHint, Location, LocationLink}
import org.eclipse.lsp4j.jsonrpc.messages

import java.util
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.SeqHasAsJava

/** Converts Go-to definition types to LSP4J types */
object GoToConverter extends StrictImplicitLogging {

  /** Convert [[SourceLocation.GoTo]]s to LSP4J's either type [[lsp4j.Location]] */
  def toLocationEither(locations: Iterable[SourceLocation.GoTo]): messages.Either[util.List[_ <: Location], util.List[_ <: LocationLink]] =
    messages.Either.forLeft[util.List[_ <: Location], util.List[_ <: LocationLink]](toLocations(locations))

  def toLocations(locations: Iterable[SourceLocation.GoTo]): util.ArrayList[Location] = {
    val javaLocations =
      GoToConverter.toLocations(locations.iterator)

    CollectionUtil.toJavaList(javaLocations)
  }

  def toInlayHint(types: ArraySeq[SourceLocation.InlayHint])(implicit logger: ClientLogger): util.List[InlayHint] =
    types
      .flatMap(toInlayHint)
      .asJava

  private def toInlayHint(hint: SourceLocation.InlayHint)(implicit logger: ClientLogger): Option[InlayHint] =
    hint.inlayHintPosition() match {
      case Some(displayPosition) =>
        val javaHint =
          new InlayHint(
            CommonConverter.toPosition(displayPosition),
            messages.Either.forLeft(hint.hint)
          )

        Some(javaHint)

      case None =>
        // None can occur when `SourceIndex` from node's AST is empty.
        // format: off
        logger.error(s"LineRange not found for hint '${hint.hint}'. SourceURI: ${hint.parsed.fileURI}. typeDef.TypeId: ${hint.typeDef.ast}, typeDef.fileURI: ${hint.typeDef.source.parsed.fileURI}")
        // format: on
        None
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
