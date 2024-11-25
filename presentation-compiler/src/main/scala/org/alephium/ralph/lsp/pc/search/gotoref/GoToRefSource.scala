// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.pc.search.CodeProvider
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
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] =
    CodeProvider
      .goToDefinition
      .search( // find definitions for the token at the given cursorIndex.
        cursorIndex = cursorIndex,
        sourceCode = sourceCode,
        workspace = workspace,
        searchSettings = settings.goToDefSetting
      )
      .flatMap {
        case SourceLocation.File(_) =>
          Iterator.empty

        case location @ SourceLocation.Node(_, _) =>
          // find references for the definitions
          goTo(
            defLocation = location,
            workspace = workspace,
            settings = settings
          )
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
      defLocation: SourceLocation.Node[Ast.Positioned],
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToRefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] = {
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
