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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceTreeInScope
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

/** Common Go-to functions */
object GoTo {

  /**
   * Executes the `searcher` function on all source-trees in scope.
   *
   * @param sourceCode The source-tree in scope.
   * @param workspace  The workspace to which this source-tree belongs.
   * @param searcher   The search function to execute.
   * @return Go-to definition search results.
   */
  def inheritedParents(
      sourceCode: SourceTreeInScope,
      workspace: WorkspaceState.IsSourceAware,
      searcher: Tree.Source => Iterator[Ast.Positioned]): Iterator[GoToLocation] =
    WorkspaceSearcher
      .collectInheritedParents(sourceCode, workspace)
      .iterator
      .flatMap {
        treeInScope =>
          // execute the searcher function on each tree
          val searchResult =
            searcher(treeInScope.tree)

          GoToLocation(
            sourceCode = treeInScope.parsed,
            asts = searchResult
          )
      }

  /**
   * Executes the `searcher` function on all children implementing
   * the given source-tree.
   *
   * @param sourceCode The source code for which implementing children are being searched.
   * @param workspace  The workspace to which this source-tree and all its implementing children belong.
   * @param searcher   The search function to execute.
   * @return Go-to definition search results.
   */
  def implementingChildren(
      sourceCode: SourceTreeInScope,
      workspace: WorkspaceState.IsSourceAware,
      searcher: Tree.Source => Iterator[Ast.Positioned]): Iterator[GoToLocation] =
    WorkspaceSearcher
      .collectImplementingChildren(sourceCode, workspace)
      .iterator
      .flatMap {
        treeInScope =>
          // execute the searcher function on each tree
          val searchResult =
            searcher(treeInScope.tree)

          GoToLocation(
            sourceCode = treeInScope.parsed,
            asts = searchResult
          )
      }

}
