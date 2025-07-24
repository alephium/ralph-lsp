// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion.multi

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.search.{CodeProvider, MultiCodeProvider}
import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.alephium.ralph.lsp.pc.PCStates
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.IsCancelled
import org.alephium.ralph.lsp.utils.URIUtil.isFileScheme

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

private[search] case object CompletionMultiCodeProvider extends MultiCodeProvider[Unit, Suggestion] with StrictImplicitLogging {

  /** @inheritdoc */
  override def search(
      fileURI: URI,
      line: Int,
      character: Int,
      isCancelled: IsCancelled,
      pcStates: PCStates,
      settings: Unit
    )(implicit logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[Suggestion]]] =
    if (isCancelled.check() || !isFileScheme(fileURI))
      Future.successful(Right(ArraySeq.empty))
    else
      Future {
        pcStates.findContains(fileURI) match {
          case Some(currentPCState) =>
            val result =
              search(
                fileURI = fileURI,
                line = line,
                character = character,
                isCancelled = isCancelled,
                workspace = currentPCState.workspace
              )

            Right(result)

          case None =>
            logger.trace(s"Code completion unsuccessful: Source not within an active workspace. fileURI: $fileURI")
            Right(ArraySeq.empty[Suggestion])
        }
      }

  /**
   * Searches for results in the workspace.
   *
   * @param fileURI     The URI of the file where this search is executed.
   * @param line        The line number where the search begins.
   * @param character   The character offset within the line.
   * @param isCancelled Check whether the search should be cancelled.
   * @param workspace   The workspace to search.
   * @return Code completion suggestions.
   */
  private def search(
      fileURI: URI,
      line: Int,
      character: Int,
      isCancelled: IsCancelled,
      workspace: WorkspaceState
    )(implicit logger: ClientLogger): ArraySeq[Suggestion] =
    if (isCancelled.check())
      ArraySeq.empty
    else
      workspace match {
        case sourceAware: WorkspaceState.IsSourceAware =>
          val completionResult =
            CodeProvider
              .codeCompleter
              .search(
                line = line,
                character = character,
                fileURI = fileURI,
                workspace = sourceAware,
                searchSettings = ()
              )

          completionResult match {
            case Some(Right(suggestions)) =>
              // completion successful
              suggestions.to(ArraySeq)

            case Some(Left(error)) =>
              // Completion failed: Log the error message
              logger.info("Code completion unsuccessful: " + error.message)
              ArraySeq.empty[Suggestion]

            case None =>
              // Not a ralph file or it does not belong to the workspace's contract-uri directory.
              ArraySeq.empty[Suggestion]
          }

        case _: WorkspaceState.Created =>
          // Workspace must be compiled at least once to enable code completion.
          // The server must've invoked the initial compilation in the boot-up initialize function.
          logger.info("Code completion unsuccessful: Workspace is not compiled")
          ArraySeq.empty[Suggestion]
      }

}
