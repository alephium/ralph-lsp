// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref.multi

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.{PCSearcher, PCStates}
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.search.gotoref.GoToRefSetting
import org.alephium.ralph.lsp.pc.search.MultiCodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.utils.IsCancelled
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

private[search] case object GoToRefMultiCodeProvider extends MultiCodeProvider[GoToRefMultiSetting, SourceLocation.GoToRefStrict] with StrictImplicitLogging {

  /**
   * Searches the reference location(s) for a symbol at the given position in a file.
   *
   * This function is used by the LSP to resolve “Go to References” requests across multiple workspaces.
   * For single-workspace search, use [[org.alephium.ralph.lsp.pc.search.gotoref.GoToRefCodeProvider]].
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
      settings: GoToRefMultiSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[SourceLocation.GoToRefStrict]]] =
    MultiCodeProvider
      .goToDef
      .search(
        fileURI = fileURI,
        line = line,
        character = character,
        isCancelled = isCancelled,
        pcStates = pcStates,
        settings = ()
      )
      .flatMap {
        case Left(error) =>
          Future.successful(Left(error))

        case Right(definitions) =>
          val refSettings =
            GoToRefSetting(
              includeDeclaration = settings.isIncludeDeclaration,
              includeTemplateArgumentOverrides = false,
              includeEventFieldReferences = true,
              goToDefSetting = GoToDefSetting(
                includeAbstractFuncDef = false,
                includeInheritance = true
              )
            )

          // Execute search in parallel for each definition across all workspaces.
          Future
            .traverse(definitions) {
              definition =>
                references(
                  definition = definition,
                  settings = refSettings,
                  isCancelled = isCancelled,
                  pcStates = pcStates
                )
            }
            .map(_.flatten)
            .map(SourceLocation.distinctByLocation)
            .map(Right(_))
      }

  /**
   * Finds all reference location(s) for a symbol at the given position in a file.
   *
   * This function is used by the LSP to resolve "Go to References" requests.
   *
   * @param definition  The definition to search for references.
   * @param isCancelled A callback to check if the request has been cancelled.
   * @param pcStates  The current state of the language server, holding all workspace states.
   * @return Either an error or a sequence of reference locations.
   */
  private def references(
      definition: SourceLocation.GoToDef,
      settings: GoToRefSetting,
      isCancelled: IsCancelled,
      pcStates: PCStates
    )(implicit searchCache: SearchCache,
      logger: ClientLogger,
      ec: ExecutionContext): Future[ArraySeq[SourceLocation.GoToRefStrict]] =
    definition.index match {
      case Some(index) =>
        val lineRange =
          StringUtil.buildLineRange(
            code = definition.parsed.code,
            from = index.from,
            to = index.to
          )

        // Execute search in parallel across all workspaces
        Future
          .traverse(pcStates.states) {
            state =>
              Future {
                PCSearcher.goTo[SourceCodeState.Parsed, GoToRefSetting, SourceLocation.GoToRefStrict](
                  fileURI = definition.parsed.fileURI,
                  line = lineRange.from.line,
                  character = lineRange.from.character,
                  searchSettings = settings,
                  isCancelled = isCancelled,
                  state = state
                )
              }
          }
          .map(_.flatten)

      case None =>
        // This occurs because `Ast.Positioned` contains optional `SourceIndex`.
        // This can be removed when the migration to `SoftAST` is complete.
        logger.error(s"GoToDef contains empty SourceIndex. File: ${definition.parsed.fileURI}")
        Future.successful(ArraySeq.empty[SourceLocation.GoToRefStrict])
    }

}
