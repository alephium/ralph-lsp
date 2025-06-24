// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.gotodef._
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

/**
 * Implements [[CodeProvider]] that provides hover results of type [[SourceLocation.Hover]].
 *
 * To execution this function invoke [[CodeProvider.search]] with [[SourceLocation.Hover]] as type parameter.
 */
private[search] case object HoverCodeProvider extends CodeProvider[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.Hover] with StrictImplicitLogging {

  /** @inheritdoc */
  override def searchLocal(
      cursorIndex: Int,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: (SoftAST.type, GoToDefSetting)
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Hover] =
    // find the statement where this cursorIndex sits.
    sourceCode.astSoft.fetch() match {
      case Left(error) =>
        // This will be removed when integration is complete,
        // when SourceCodeState.ErrorParser responds to SoftParser errors.
        // Note: SoftParser is not expected to fail given any input, so this is less likely to occur.
        //       Log it for now.
        logger.error {
          s"""SoftParser Error: Failed to parse source code.
             |File: ${sourceCode.fileURI}
             |Error Message: ${error.message}""".stripMargin
        }

        Iterator.empty

      case Right(softAST) =>
        CodeProvider
          .goToDefSoft
          .searchLocal(
            cursorIndex = cursorIndex,
            sourceCode = sourceCode,
            workspace = workspace,
            searchSettings = searchSettings
          )
          .flatMap {
            case SourceLocation.File(parsedFile) =>
              logger.info(s"Hover requested not implemented for imports: ${parsedFile.fileURI}")
              None

            case SourceLocation.NodeSoft(definition, _) =>
              softAST.toNode.findAtIndex(definition.index) match { // Find the exact location of the definition.
                case Some(definition) =>
                  definition
                    .walkParents
                    .collectFirst { // Find the nearest parent.
                      // TODO add cases here
                      case _ =>
                        None
                    }
                    .flatten

                case None =>
                  None
              }
          }
    }

}
