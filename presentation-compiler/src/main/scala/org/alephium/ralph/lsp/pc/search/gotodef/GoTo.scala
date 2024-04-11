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
  def inScope(
      sourceCode: SourceTreeInScope,
      workspace: WorkspaceState.IsSourceAware,
      searcher: Tree.Source => Iterator[Ast.Positioned]): Iterator[GoToLocation] =
    WorkspaceSearcher
      .collectInScope(sourceCode, workspace) // collect all source-files/source-trees in scope
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
