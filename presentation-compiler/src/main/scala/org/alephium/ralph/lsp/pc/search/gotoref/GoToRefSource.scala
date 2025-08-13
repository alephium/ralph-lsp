// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

private object GoToRefSource extends StrictImplicitLogging {

  /**
   * Navigates to the references of a token in the source code.
   *
   * @param cursorIndex The index of the token selected.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param workspace   The workspace where this search was executed and where all the source trees exist.
   * @param settings    Search settings.
   * @return An iterator reference location(s).
   */
  def goTo(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    CodeProvider
      .goToDefSoft
      .searchLocal( // find definitions for the token at the given cursorIndex.
        cursorIndex = cursorIndex,
        sourceCode = sourceCode,
        workspace = workspace,
        searchSettings = (SoftAST, settings.goToDefSetting)
      )
      .flatMap {
        case SourceLocation.File(_) =>
          Iterator.empty

        case location @ SourceLocation.NodeSoft(ast, _) =>
          location.toNodeStrict() match {
            case Some(location) =>
              // find references for the definitions
              goTo(
                defLocation = location,
                workspace = workspace,
                settings = settings
              )

            case None =>
              logger.trace(s"Strict not found for AST: ${ast.getClass.getSimpleName}")
              Iterator.empty
          }
      }
      .distinctBy { // There could be multiple definitions for a reference which could result in duplicates.
        node =>     // Ensure duplicates are removed.
          (node, node.ast.sourceIndex)
      }

  /**
   * Navigates to the references of a token in the source code.
   *
   * @param defLocation The definition/declaration node of the token selected.
   * @param workspace   The workspace where this search was executed and where all the source trees exist.
   * @param settings    Search settings.
   * @return An iterator reference location(s).
   */
  def goTo(
      defLocation: SourceLocation.NodeStrict[Ast.Positioned],
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] = {
    val defNode =
      defLocation
        .source
        .tree
        .rootNode
        .walkDown
        .find(_.data eq defLocation.ast) // find the node where this definition belongs.

    // Execute go-to references
    defNode match {
      case Some(defNode @ Node(ident: Ast.Ident, _)) =>
        GoToRefIdent.goTo(
          definition = defNode.upcast(ident),
          sourceCode = defLocation.source,
          workspace = workspace,
          settings = settings
        )

      case Some(defNode @ Node(ident: Ast.FuncId, _)) =>
        GoToRefFuncId.goTo(
          definition = defNode.upcast(ident),
          sourceCode = defLocation.source,
          workspace = workspace,
          settings = settings
        )

      case Some(defNode @ Node(ident: Ast.TypeId, _)) =>
        GoToRefTypeId.goTo(
          definition = defNode.upcast(ident),
          sourceCode = defLocation.source,
          workspace = workspace,
          settings = settings
        )

      case Some(Node(ast, _)) =>
        logger.trace(s"No GoToRef implementation for '${ast.getClass.getSimpleName}'")
        Iterator.empty

      case None =>
        logger.trace(s"Node not found for AST '${defLocation.ast.getClass.getSimpleName}' at source index '${defLocation.ast.sourceIndex}'")
        Iterator.empty
    }
  }

}
