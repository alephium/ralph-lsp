// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.inlayhints.multi

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, LinePosition}
import org.alephium.ralph.lsp.pc.PCStates
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.PCSearcher.goTo
import org.alephium.ralph.lsp.pc.search.MultiCodeProvider
import org.alephium.ralph.lsp.utils.IsCancelled
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

private[search] case object InlayHintsMultiCodeProvider extends MultiCodeProvider[LinePosition, SourceLocation.InlayHint] with StrictImplicitLogging {

  /** @inheritdoc */
  override def search(
      fileURI: URI,
      line: Int,
      character: Int,
      isCancelled: IsCancelled,
      pcStates: PCStates,
      settings: LinePosition
    )(implicit logger: ClientLogger,
      ec: ExecutionContext): Future[Either[CompilerMessage.Error, ArraySeq[SourceLocation.InlayHint]]] =
    pcStates.get(fileURI) match {
      case Left(error) =>
        // Dependency files are not part of the active workspace but should still support inlay hints.
        // For now, instead of triggering an IDE error notification, log the reason.
        logger.info(s"Inlay hints are not available for this file. Reason: ${error.message}.")
        Future.successful(Right(ArraySeq.empty))

      case Right(pcState) =>
        val result =
          goTo[SourceCodeState.Parsed, LinePosition, SourceLocation.InlayHint](
            fileURI = fileURI,
            line = line,
            character = character,
            searchSettings = settings,
            isCancelled = isCancelled,
            state = pcState
          ).to(ArraySeq)

        Future.successful(Right(result))
    }

}
