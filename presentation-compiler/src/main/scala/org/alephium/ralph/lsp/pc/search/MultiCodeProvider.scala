// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, LinePosition}
import org.alephium.ralph.lsp.pc.PCStates
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.search.completion.multi.CompletionMultiCodeProvider
import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.alephium.ralph.lsp.pc.search.gotodef.multi.GoToDefMultiCodeProvider
import org.alephium.ralph.lsp.pc.search.gotoref.multi.{GoToRefMultiCodeProvider, GoToRefMultiSetting}
import org.alephium.ralph.lsp.pc.search.inlayhints.multi.InlayHintsMultiCodeProvider
import org.alephium.ralph.lsp.pc.search.hover.multi.HoverMultiCodeProvider
import org.alephium.ralph.lsp.pc.search.rename.multi.GoToRenameMultiCodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.IsCancelled
import org.alephium.ralph.lsp.utils.log.ClientLogger

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

/**
 * A trait representing a code provider that performs search operations
 * across multiple workspaces.
 *
 * @tparam I The type of search settings or configuration.
 * @tparam O The type of search results.
 */
trait MultiCodeProvider[I, O] {

  /**
   * Searches for results in the given file and position.
   *
   * @param fileURI          The URI of the file where this search is executed.
   * @param line             The line number where the search begins.
   * @param character        The character offset within the line.
   * @param enableSoftParser Whether to use a soft parser.
   * @param isCancelled      Check whether the search should be cancelled.
   * @param pcStates         Current presentation-compiler states of each workspace.
   * @param settings         Provider-specific settings.
   *
   * @return Either an error or search results.
   */
  def search(
      fileURI: URI,
      line: Int,
      character: Int,
      enableSoftParser: Boolean,
      isCancelled: IsCancelled,
      pcStates: PCStates,
      settings: I
    )(implicit searchCache: SearchCache,
      logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[O]]]

}

object MultiCodeProvider {

  implicit val goToDef: MultiCodeProvider[Unit, SourceLocation.GoToDef] =
    GoToDefMultiCodeProvider

  implicit val goToRef: MultiCodeProvider[GoToRefMultiSetting, SourceLocation.GoToRefStrict] =
    GoToRefMultiCodeProvider

  implicit val rename: MultiCodeProvider[Unit, SourceLocation.GoToRenameStrict] =
    GoToRenameMultiCodeProvider

  implicit val completion: MultiCodeProvider[Unit, Suggestion] =
    CompletionMultiCodeProvider

  implicit val inlayHints: MultiCodeProvider[LinePosition, SourceLocation.InlayHint] =
    InlayHintsMultiCodeProvider

  implicit val hover: MultiCodeProvider[Unit, SourceLocation.Hover] =
    HoverMultiCodeProvider

}
