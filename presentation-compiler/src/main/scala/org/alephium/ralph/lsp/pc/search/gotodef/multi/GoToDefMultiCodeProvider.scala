// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef.multi

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.PCStates
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.PCSearcher.goTo
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.search.MultiCodeProvider
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
   * For single-workspace search, use [[org.alephium.ralph.lsp.pc.search.gotodef.soft.GoToDefCodeProviderSoft]].
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
    )(implicit logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[SourceLocation.GoToDef]]] = {
    val settings =
      GoToDefSetting(
        includeAbstractFuncDef = false,
        includeInheritance = true
      )

    pcStates.getOneOrAll(fileURI) match {
      case Left(error) =>
        Future.successful(Left(error))

      case Right(currentStates) =>
        Future
          .traverse(currentStates) {
            state =>
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
          }
          .map(_.flatten)
          .map(Right(_))
    }
  }

}
