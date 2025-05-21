// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.URIUtil.isFileScheme
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.IsCancelled

import java.net.URI

object PCSearcher extends StrictImplicitLogging {

  /**
   * Executes Go-to services.
   *
   * @param fileURI        URI of the file where this request was executed.
   * @param line           Line number within the file where this request was executed.
   * @param character      Character number within the Line where this request was executed.
   * @param searchSettings Settings defined for the input [[CodeProvider]].
   * @param isCancelled    Cancellation support instance.
   * @param state          Current presentation-compiler state.
   * @param provider       Target [[CodeProvider]] to use for responding to this request.
   * @param logger         Remote client and local logger.
   * @tparam I The type of input [[CodeProvider]] settings.
   * @tparam O The type of [[CodeProvider]] function output.
   * @return Go-to search results.
   */
  def goTo[S, I, O <: SourceLocation.GoTo](
      fileURI: URI,
      line: Int,
      character: Int,
      searchSettings: I,
      isCancelled: IsCancelled,
      state: PCState
    )(implicit provider: CodeProvider[S, I, O],
      logger: ClientLogger): Iterator[O] =
    if (!isFileScheme(fileURI) || isCancelled.check())
      Iterator.empty
    else
      state.workspace match {
        case sourceAware: WorkspaceState.IsSourceAware =>
          val goToResult =
            provider.search(
              line = line,
              character = character,
              fileURI = fileURI,
              workspace = sourceAware,
              searchSettings = searchSettings
            )

          if (isCancelled.check())
            Iterator.empty
          else
            goToResult match {
              case Some(Right(goToLocations)) =>
                // successful
                goToLocations

              case Some(Left(error)) =>
                // Go-to definition failed: Log the error message
                logger.info(s"${provider.productPrefix} unsuccessful: " + error.message)
                Iterator.empty

              case None =>
                // Not a ralph file, or it does not belong to the workspace's contract-uri directory.
                Iterator.empty
            }

        case _: WorkspaceState.Created =>
          // Workspace must be compiled at least once to enable GoTo definition.
          // The server must've invoked the initial compilation in the boot-up initialize function.
          logger.info(s"${provider.productPrefix} unsuccessful: Workspace is not compiled")
          Iterator.empty
      }

}
