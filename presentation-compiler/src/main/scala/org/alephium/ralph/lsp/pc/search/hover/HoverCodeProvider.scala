// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.search.gotodef._
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.Node
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
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.Hover] =
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

        case SourceLocation.NodeSoft(definition, code) =>
          code.part.toNode.findAtIndex(definition.index) match { // Find the exact location of the definition.
            case Some(definition) =>
              definition
                .walkParents
                .collectFirst { // Find the nearest parent.
                  case Node(declaration: SoftAST.DeclarationAST, _) =>
                    HoverDeclaration(declaration, code)

                  case Node(declaration: SoftAST.ExpressionAST, _) =>
                    HoverExpression(declaration, code, workspace)
                }
                .flatten

            case None =>
              None
          }
      }

}
