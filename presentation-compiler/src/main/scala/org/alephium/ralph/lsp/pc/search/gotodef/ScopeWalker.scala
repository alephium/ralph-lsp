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
import org.alephium.ralph.lsp.access.compiler.ast.AstExtra
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra

import scala.collection.mutable.ListBuffer

private object ScopeWalker {

  /**
   * Navigates the nodes within the scope of the `anchor` node, starting from the `from` node.
   *
   * @param from   The node where the search starts.
   * @param anchor The node which is being scoped and where the search ends.
   *               If the collected result is empty, nodes after the `anchor`'s position
   *               are processed until at least one item is collected.
   * @param pf     Only the Nodes defined by this partial function are collected.
   * @return Nodes within the scope of the anchor AST.
   */
  def walk[T](
      from: Node[Ast.Positioned, Ast.Positioned],
      anchor: Ast.Positioned
    )(pf: PartialFunction[Node[Ast.Positioned, Ast.Positioned], T]): Iterable[T] = {
    val found  = ListBuffer.empty[T]
    var walker = from.walkDown

    while (walker.hasNext)
      walker.next() match {
        // Check: Is this a scoped node that does not contain the anchor node within its scope? If yes, drop all its child nodes.
        // format: off
        case block @ Node(_: Ast.While[_] | _: Ast.ForLoop[_] | _: Ast.IfBranch[_] | _: Ast.ElseBranch[_], _) if !SourceIndexExtra.contains(block.data.sourceIndex, anchor.sourceIndex) =>
        // format: on
          walker = walker dropWhile { // drop all child nodes
            next =>
              SourceIndexExtra.contains(
                parent = block.data.sourceIndex,
                child = next.data.sourceIndex
              )
          }

        // Check:
        // - Is this node (i.e., within the scope) defined by the partial-function?
        // - And is it before the anchor node?
        // - If it's defined after the anchor node (node in scope), then only add it if currently collected items are empty.
        case node @ Node(ast, _) if pf.isDefinedAt(node) && (AstExtra.isBehind(ast, anchor) || found.isEmpty) =>
          found addOne pf(node)

        case _ =>
        // ignore the rest
      }

    found

  }

}
