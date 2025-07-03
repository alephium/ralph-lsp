// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover.multi

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.PCStates
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.PCSearcher.goTo
import org.alephium.ralph.lsp.pc.search.MultiCodeProvider
import org.alephium.ralph.lsp.utils.IsCancelled
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

private[search] case object HoverMultiCodeProvider extends MultiCodeProvider[Unit, SourceLocation.Hover] with StrictImplicitLogging {

  /**
   * Searches for hover information at the specified position in a file.
   *
   * This function is used by the LSP to resolve hover requests across multiple workspaces.
   * For single-workspace search, use [[org.alephium.ralph.lsp.pc.search.hover.HoverCodeProvider]].
   *
   * @param fileURI          The URI of the file where this search is executed.
   * @param line             The line number where the search begins.
   * @param character        The character offset within the line.
   * @param enableSoftParser Whether to use a soft parser.
   * @param isCancelled      Check whether the search should be cancelled.
   * @param pcStates         Current presentation-compiler states of each workspace.
   * @param settings         Provider-specific settings.
   * @return Either an error or searched hover results.
   */
  override def search(
      fileURI: URI,
      line: Int,
      character: Int,
      enableSoftParser: Boolean,
      isCancelled: IsCancelled,
      pcStates: PCStates,
      settings: Unit
    )(implicit logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[SourceLocation.Hover]]] = {
    /*
     * Hover information relies on the definition of the symbol at the cursor position.
     * Therefore, `GoToDef` is used to find the definition first
     */
    val settings =
      GoToDefSetting(
        includeAbstractFuncDef = false,
        includeInheritance = true
      )

    if (enableSoftParser) {
      pcStates.getOneOrAll(fileURI) match {
        case Left(error) =>
          Future.successful(Left(error))

        case Right(currentStates) =>
          val hovers = currentStates.flatMap {
            state =>
              goTo[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.Hover](
                fileURI = fileURI,
                line = line,
                character = character,
                searchSettings = (SoftAST, settings),
                isCancelled = isCancelled,
                state = state
              )
          }

          val distinct = SourceLocation.distinctByLocation(hovers)

          Future.successful(Right(distinct))
      }
    } else {
      logger.debug("Hover search is not implemented yet for strict parsing")
      Future.successful(Right(ArraySeq.empty[SourceLocation.Hover]))
    }
  }

}
