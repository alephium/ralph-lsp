// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.{Ast, SourceIndex}
import org.alephium.ralph.lsp.access.compiler.ast.AstExtra
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.utils.Node

import scala.collection.mutable.ListBuffer

private[search] object ScopeWalker {

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

  /**
   * Note: This is a direct clone of the above `walk` function for strict-ast.
   *
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
      from: Node[SoftAST, SoftAST],
      anchor: SourceIndex
    )(pf: PartialFunction[Node[SoftAST, SoftAST], Iterator[T]]): Iterable[T] = {
    val found = ListBuffer.empty[T]

    val iterator =
      from filterDown {
        case Node(_: SoftAST.Space | _: SoftAST.Comments | _: SoftAST.Comment, _) =>
          // Quickly skip spaces and comments since no service processes them.
          // This drops the node and its children in the same iteration, efficient, especially for multiline comments.
          false

        case block @ Node(_: SoftAST.While | _: SoftAST.For | _: SoftAST.IfElse | _: SoftAST.Else | _: SoftAST.Block, _) if !block.data.contains(anchor) =>
          // drop all child nodes
          false

        // Check:
        // - Is this node (i.e. within the scope) defined by the partial-function?
        // - If it is a declaration, process the node because declarations do not require ordering and are available locally to all code.
        // - If it is not a declaration, is it before the anchor node?
        // - If it is defined after the anchor node (node in scope), then only add it if currently collected items are empty.
        case node @ Node(ast, _) if pf.isDefinedAt(node) && (ast.isInstanceOf[SoftAST.DeclarationAST] || ast.isBehind(anchor) || found.isEmpty) =>
          found addAll pf(node)
          // This node is processed, drop all its children.
          false

        case _ =>
          // Keep processing the rest
          true
      }

    // run the iterator
    while (iterator.hasNext)
      iterator.next()

    found

  }

}
