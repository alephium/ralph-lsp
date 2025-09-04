// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.utils.Node

private[search] object ScopeWalker {

  /**
   * Filters and collects the nodes within the scope of the `anchor` node, starting from the `from` node.
   *
   * @param from   The node where the search starts.
   * @param anchor The node which is being scoped and where the search ends.
   *               If the collected result is empty, nodes after the `anchor`'s position
   *               are processed until at least one item is collected.
   * @param pf     Only the Nodes defined by this partial function are collected.
   * @return Nodes within the scope of the anchor [[SourceIndex]] that are also defined by the [[PartialFunction]].
   */
  def collect[T](
      from: Node[SoftAST, SoftAST],
      anchor: SourceIndex
    )(pf: PartialFunction[Node[SoftAST, SoftAST], Iterator[T]]): Iterator[T] = {
    // Indicates if the `collect` returned at least one element
    var isResultEmpty = true

    val collected =
      from collect {
        case Node(_: SoftAST.Space | _: SoftAST.Comments | _: SoftAST.Comment, _) =>
          // Quickly skip spaces and comments since no service processes them.
          // This drops the node and its children in the same iteration, efficient, especially for multiline comments.
          Iterator.empty

        case block @ Node(_: SoftAST.While | _: SoftAST.For | _: SoftAST.IfElse | _: SoftAST.Else | _: SoftAST.Block, _) if !block.data.contains(anchor) =>
          // drop all child nodes
          Iterator.empty

        // Check:
        // - Is this node (i.e. within the scope) defined by the partial-function?
        // - If it is a declaration, process the node because declarations do not require ordering and are available locally to all code.
        // - If it is not a declaration, is it before the anchor node?
        // - If it is defined after the anchor node (node in scope), then only add it if currently collected items are empty.
        case node @ Node(ast, _) if pf.isDefinedAt(node) && (ast.isInstanceOf[SoftAST.DeclarationAST] || ast.isBehind(anchor) || isResultEmpty) =>
          // The caller owns this node, collect its results.
          val result = pf(node)
          // Update the flag checking if at least one element exist
          if (isResultEmpty)
            isResultEmpty = result.isEmpty
          // save the resulting iterator
          result
      }

    collected.flatten
  }

}
