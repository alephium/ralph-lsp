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

package org.alephium.ralph.lsp.access.compiler.ast.node

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.utils.Node

/** Functions that build a traversable tree from [[Ast.ContractWithState]], returning the root [[Node]]. */
object NodeBuilder extends StrictLogging {

  /**
   * Given an [[Ast.ContractWithState]], builds a traversable tree.
   *
   * @param ast The [[Ast.ContractWithState]] instance
   * @return Root node of the tree.
   */
  def buildRootNode(ast: Ast.GlobalDefinition): Node[Ast.GlobalDefinition, Ast.Positioned] = {
    // TODO: Are all these siblings? If they are not, they need to build a tree structure using source-index.
    val rootSiblings =
      ast match {
        case ast: Ast.TxScript =>
          Seq(buildParent(ast.ident)) ++
            buildParents(ast.templateVars) ++
            buildParents(ast.funcs)

        case ast: Ast.Contract =>
          buildParents(ast.stdInterfaceId.toSeq) ++
            Seq(buildParent(ast.ident)) ++
            buildParents(ast.templateVars) ++
            buildParents(ast.fields) ++
            buildParents(ast.funcs) ++
            buildParents(ast.events) ++
            buildParents(ast.constantVars) ++
            buildParents(ast.enums) ++
            buildParents(ast.inheritances) ++
            buildParents(ast.maps)

        case ast: Ast.ContractInterface =>
          buildParents(ast.stdId.toSeq) ++
            Seq(buildParent(ast.ident)) ++
            buildParents(ast.funcs) ++
            buildParents(ast.events) ++
            buildParents(ast.inheritances)

        case ast: Ast.Struct =>
          buildParent(ast.id) +:
            buildParents(ast.fields)

        case ast: Ast.EnumDef[_] =>
          buildParent(ast.id) +:
            buildParents(ast.fields)

        case ast: Ast.ConstantVarDef[_] =>
          Seq(buildParent(ast))

        case _: Ast.AssetScript =>
          // AssetScript is not parsed. This will be supported in the future.
          List.empty
      }

    // sort the sibling according to their source-index i.e. following their order of position in code.
    val sortedRootSiblings =
      rootSiblings.sortBy(_.data.sourceIndex.map(_.index))

    // Root node
    Node(
      data = ast,
      children = sortedRootSiblings
    )
  }

  /**
   * Constructs a parent [[Node]] for the given positioned AST parent.
   *
   * @param parent The positioned AST node to process.
   * @return A Node representing the given parent and its children.
   */
  private def buildParent(parent: Ast.Positioned): Node[Positioned, Positioned] = {
    val children = processChildren(parent)
    Node(parent, children)
  }

  /**
   * Processes the children of the given parent node as independent parent nodes.
   *
   * @param parent The parent node whose children are to be processed.
   * @return A list of nodes representing the children of the given parent node.
   */
  private def processChildren(parent: Any): List[Node[Ast.Positioned, Ast.Positioned]] =
    parent match {
      case product: Product =>
        product
          .productIterator
          .toList
          .collect(processParent)
          .flatten

      case item =>
        logger.trace(s"Not a product: $item: ${item.getClass}")
        List.empty
    }

  /**
   * Constructs parent [[Node]] objects for each object of type [[Ast.Positioned]] in the given sequence of products.
   *
   * @param products A sequence of items to process, each representing a parent node.
   * @return A sequence of Nodes, each representing a parent node.
   */
  private def buildParents(products: Seq[Any]): Seq[Node[Ast.Positioned, Ast.Positioned]] =
    products
      .collect(processParent)
      .flatten

  /**
   * Processes a product containing various types within an AST, checking for the existence
   * of an [[Ast.Positioned]] type and processing it accordingly.
   *
   * @return A partial function that processes input of any type and returns a sequence of Nodes,
   *         each representing a parent node.
   */
  private def processParent: PartialFunction[Any, Seq[Node[Ast.Positioned, Ast.Positioned]]] = {
    case parent: Positioned =>
      List(buildParent(parent))

    case (left: Positioned, right: Positioned) =>
      List(
        buildParent(left),
        buildParent(right)
      )

    case positions: Seq[_] =>
      buildParents(positions)

    case Some(product) =>
      processParent(product)

    case Right(product) =>
      processParent(product)

    case Left(product) =>
      processParent(product)

    case other =>
      logger.trace(s"Not a Positioned instance: ${other.getClass}")
      Seq.empty
  }

}
