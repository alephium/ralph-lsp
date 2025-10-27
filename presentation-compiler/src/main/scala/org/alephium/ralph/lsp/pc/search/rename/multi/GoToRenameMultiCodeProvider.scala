// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename.multi

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.{PCSearcher, PCStates}
import org.alephium.ralph.lsp.pc.search.MultiCodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.IsCancelled

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

case object GoToRenameMultiCodeProvider extends MultiCodeProvider[Unit, SourceLocation.GoToRenameSoft] with StrictImplicitLogging {

  /**
   * Searches for results to rename.
   *
   * @param fileURI     The URI of the file where this search is executed.
   * @param line        The line number where the search begins.
   * @param character   The character offset within the line.
   * @param isCancelled Check whether the search should be cancelled.
   * @param pcStates    Current presentation-compiler states of each workspace.
   * @param settings    Provider-specific settings.
   * @return Either an error or search results.
   */
  override def search(
      fileURI: URI,
      line: Int,
      character: Int,
      isCancelled: IsCancelled,
      pcStates: PCStates,
      settings: Unit
    )(implicit searchCache: SearchCache,
      logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[SourceLocation.GoToRenameSoft]]] =
    Future {
      pcStates.findContains(fileURI) match {
        case Some(state) =>
          val result =
            PCSearcher
              .goTo[SourceCodeState.Parsed, Unit, SourceLocation.GoToRenameSoft](
                fileURI = fileURI,
                line = line,
                character = character,
                searchSettings = (),
                isCancelled = isCancelled,
                state = state
              )
              .to(ArraySeq)

          Right(result)

        case None =>
          logger.info(s"Operation blocked: Renaming within files outside the active workspace is not allowed. Affected file: $fileURI")
          Right(ArraySeq.empty)
      }
    }

}
