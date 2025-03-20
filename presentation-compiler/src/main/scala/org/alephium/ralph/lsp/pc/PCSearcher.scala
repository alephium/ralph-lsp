// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.URIUtil.isFileScheme
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.IsCancelled

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

object PCSearcher extends StrictImplicitLogging {

  /**
   * Finds the definition location(s) for a symbol at the given position in a file.
   *
   * This function is used by the LSP to resolve "Go to Definition" requests.
   *
   * @param fileURI          The `URI` of the file where the lookup is performed.
   * @param line             The line number (0-based) where the symbol is located.
   * @param character        The character position (0-based) in the line.
   * @param enableSoftParser Whether to use the soft parser for fault-tolerant parsing.
   * @param isCancelled      A callback to check if the request has been cancelled.
   * @param pcStates         The current state of the language server, holding all workspace states.
   * @return A `Future` containing either an error or a sequence of definition locations.
   */
  def definition(
      fileURI: URI,
      line: Int,
      character: Int,
      enableSoftParser: Boolean,
      isCancelled: IsCancelled,
      pcStates: PCStates
    )(implicit logger: ClientLogger,
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

  /**
   * Executes Go-to services.
   *
   * @param fileURI        URI of the file where this request was executed.
   * @param line           Line number within the file where this request was executed.
   * @param character      Character number within the Line where this request was executed.
   * @param searchSettings Settings defined for the input [[CodeProvider]].
   * @param isCancelled    Cancellation support instance.
   * @param state          Current presentation-compiler state.
   * @param codeProvider   Target [[CodeProvider]] to use for responding to this request.
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
    )(implicit codeProvider: CodeProvider[S, I, O],
      logger: ClientLogger): Iterator[O] =
    if (!isFileScheme(fileURI) || isCancelled.check())
      Iterator.empty
    else
      state.workspace match {
        case sourceAware: WorkspaceState.IsSourceAware =>
          val goToResult =
            CodeProvider.search[S, I, O](
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
                logger.info(s"${codeProvider.productPrefix} unsuccessful: " + error.message)
                Iterator.empty

              case None =>
                // Not a ralph file, or it does not belong to the workspace's contract-uri directory.
                Iterator.empty
            }

        case _: WorkspaceState.Created =>
          // Workspace must be compiled at least once to enable GoTo definition.
          // The server must've invoked the initial compilation in the boot-up initialize function.
          logger.info(s"${codeProvider.productPrefix} unsuccessful: Workspace is not compiled")
          Iterator.empty
      }

}
