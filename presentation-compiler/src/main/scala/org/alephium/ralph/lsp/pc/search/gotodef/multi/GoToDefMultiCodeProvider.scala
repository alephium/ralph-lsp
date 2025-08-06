// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef.multi

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.{PCState, PCStates}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.PCSearcher.goTo
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.search.MultiCodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.utils.IsCancelled
import org.alephium.ralph.lsp.utils.log.ClientLogger

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

private[search] case object GoToDefMultiCodeProvider extends MultiCodeProvider[Unit, SourceLocation.GoToDef] {

  /**
   * Searches the definition location(s) for a symbol at the given position in a file.
   *
   * This function is used by the LSP to resolve “Go to Definition” requests across multiple workspaces.
   * For single-workspace search, use [[org.alephium.ralph.lsp.pc.search.gotodef.GoToDefCodeProvider]].
   *
   * @param fileURI          The URI of the file where this search is executed.
   * @param line             The line number where the search begins.
   * @param character        The character offset within the line.
   * @param enableSoftParser Whether to use a soft parser.
   * @param isCancelled      Check whether the search should be cancelled.
   * @param pcStates         Current presentation-compiler states of each workspace.
   * @param settings         Provider-specific settings.
   * @return Either an error or search results.
   */
  override def search(
      fileURI: URI,
      line: Int,
      character: Int,
      enableSoftParser: Boolean,
      isCancelled: IsCancelled,
      pcStates: PCStates,
      settings: Unit
    )(implicit searchCache: SearchCache,
      logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[SourceLocation.GoToDef]]] = {
    val settings =
      GoToDefSetting(
        includeAbstractFuncDef = false,
        includeInheritance = true
      )

    def searchSoft(state: PCState) =
      Future {
        goTo[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.GoToDefSoft](
          fileURI = fileURI,
          line = line,
          character = character,
          searchSettings = (SoftAST, settings),
          isCancelled = isCancelled,
          state = state
        )
      }

    def searchStrict(state: PCState) =
      Future {
        goTo[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict](
          fileURI = fileURI,
          line = line,
          character = character,
          searchSettings = settings,
          isCancelled = isCancelled,
          state = state
        )
      }

    pcStates.getOneOrAll(fileURI) match {
      case Left(error) =>
        Future.successful(Left(error))

      case Right(currentStates) =>
        // Compute soft AST locations asynchronously
        val softLocations =
          if (enableSoftParser)
            currentStates map searchSoft
          else
            ArraySeq.empty

        // Compute strict AST locations within the current thread
        val strictLocations =
          currentStates map searchStrict

        for {
          soft   <- Future.sequence(softLocations)
          strict <- Future.sequence(strictLocations)
        } yield {
          val softFlattened   = soft.flatten
          val strictFlattened = strict.flatten
          val distinct        = SourceLocation.distinctByLocation(softFlattened ++ strictFlattened)
          Right(distinct)
        }
    }
  }

}
