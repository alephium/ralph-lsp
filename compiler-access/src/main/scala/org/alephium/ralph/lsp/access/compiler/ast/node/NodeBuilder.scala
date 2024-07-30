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

/** Functions that build a traversable tree from [[Ast.ContractWithState]], returning the root [[Node]]. */
object NodeBuilder extends StrictLogging {

  /**
   * Given an [[Ast.ContractWithState]], builds a traversable tree.
   *
   * @param ast The [[Ast.ContractWithState]] instance
   * @return Root node of the tree.
   */
  def buildRootNode(ast: Ast.MultiContractDef): Node[Ast.MultiContractDef, Ast.Positioned] = {
    // TODO: Are all these siblings? If they are not, they need to build a tree structure using source-index.
    val rootSiblings =
      ast match {
        case ast: Ast.TxScript =>
          buildOne(ast.ident) ++
            buildMany(ast.templateVars) ++
            buildMany(ast.funcs)

        case ast: Ast.Contract =>
          buildOne(ast.stdInterfaceId) ++
            buildOne(ast.ident) ++
            buildMany(ast.templateVars) ++
            buildMany(ast.fields) ++
            buildMany(ast.funcs) ++
            buildMany(ast.events) ++
            buildMany(ast.constantVars) ++
            buildMany(ast.enums) ++
            buildMany(ast.inheritances) ++
            buildMany(ast.maps)

        case ast: Ast.ContractInterface =>
          buildOne(ast.stdId) ++
            buildOne(ast.ident) ++
            buildMany(ast.funcs) ++
            buildMany(ast.events) ++
            buildMany(ast.inheritances)

        case ast: Ast.Struct =>
          buildOne(ast.id) ++
            buildMany(ast.fields)

        case ast: Ast.EnumDef[_] =>
          buildOne(ast.id) ++
            buildMany(ast.fields)

        case ast: Ast.ConstantVarDef[_] =>
          buildOne(ast)
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

  private def buildOne(product: Any): List[Node[Ast.Positioned, Ast.Positioned]] =
    product match {
      case product: Product =>
        product
          .productIterator
          .toList
          .collect(positionedProducts)
          .flatten

      case item =>
        logger.trace(s"Not a product: $item: ${item.getClass}")
        List.empty
    }

  private def buildMany(products: Seq[Any]): Seq[Node[Ast.Positioned, Ast.Positioned]] =
    products
      .collect(positionedProducts)
      .flatten

  private def positionedProducts: PartialFunction[Any, Seq[Node[Ast.Positioned, Ast.Positioned]]] = {
    case positioned: Positioned =>
      val children = buildOne(positioned)
      List(Node(positioned, children))

    case (left: Positioned, right: Positioned) =>
      val leftChildren  = buildOne(left)
      val rightChildren = buildOne(right)
      List(
        Node(left, leftChildren),
        Node(right, rightChildren)
      )

    case Some(positioned: Positioned) =>
      positionedProducts(positioned)

    case Some(tuple @ (_: Positioned, _: Positioned)) =>
      positionedProducts(tuple)

    case positions: Seq[_] =>
      buildMany(positions)

    case Some(positions: Seq[_]) =>
      buildMany(positions)
  }

}
