// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.PCStates
import org.alephium.ralph.lsp.pc.search.gotodef.multi.GoToDefMultiCodeProvider
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
    )(implicit logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[O]]]

}

object MultiCodeProvider {

  implicit val goToDef: MultiCodeProvider[Unit, SourceLocation.GoToDef] =
    GoToDefMultiCodeProvider

  /**
   * Executes the search.
   */
  def search[I, O](
      fileURI: URI,
      line: Int,
      character: Int,
      enableSoftParser: Boolean,
      isCancelled: IsCancelled,
      pcStates: PCStates,
      settings: I
    )(implicit provider: MultiCodeProvider[I, O],
      logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[O]]] =
    provider.search(
      fileURI = fileURI,
      line = line,
      character = character,
      enableSoftParser = enableSoftParser,
      isCancelled = isCancelled,
      pcStates = pcStates,
      settings = settings
    )

}
