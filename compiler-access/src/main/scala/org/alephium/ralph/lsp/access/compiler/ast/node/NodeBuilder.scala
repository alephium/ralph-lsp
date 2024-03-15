package org.alephium.ralph.lsp.access.compiler.ast.node

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.{Ast, SourceIndex}

/** Functions that build a traversable tree from [[Ast.ContractWithState]], returning the root [[Node]]. */
object NodeBuilder extends StrictLogging {

  /**
   * Given an [[Ast.ContractWithState]], builds a traversable tree.
   *
   * @param ast The [[Ast.ContractWithState]] instance
   * @return Root node of the tree.
   */
  def buildRootNode(ast: Either[Ast.ContractWithState, Ast.Struct], rootIndex: SourceIndex): Node[Positioned] = {
    // TODO: Are all these siblings? If they are not, they need to build a tree structure using source-index.
    val rootSiblings =
      ast match {
        case Left(ast: Ast.TxScript) =>
          buildOne(ast.ident) ++
            buildMany(ast.templateVars) ++
            buildMany(ast.funcs)

        case Left(ast: Ast.Contract) =>
          buildOne(ast.stdInterfaceId) ++
            buildOne(ast.ident) ++
            buildMany(ast.templateVars) ++
            buildMany(ast.fields) ++
            buildMany(ast.funcs) ++
            buildMany(ast.events) ++
            buildMany(ast.constantVars) ++
            buildMany(ast.enums) ++
            buildMany(ast.inheritances)

        case Left(ast: Ast.ContractInterface) =>
          buildOne(ast.stdId) ++
            buildOne(ast.ident) ++
            buildMany(ast.funcs) ++
            buildMany(ast.events) ++
            buildMany(ast.inheritances)

        case Right(ast: Ast.Struct) =>
          buildOne(ast.id) ++
            buildMany(ast.fields)
      }

    // sort the sibling according to their source-index i.e. following their order of position in code.
    val sortedRootSiblings =
      rootSiblings.sortBy(_.data.sourceIndex.map(_.index))

    // Root node
    Node(
      data = RootPosition(rootIndex),
      children = sortedRootSiblings
    )
  }

  private def buildOne(product: Any): List[Node[Positioned]] =
    product match {
      case product: Product =>
        product.productIterator.toList
          .collect(positionedProducts)
          .flatten

      case item =>
        logger.trace(s"Not a product: $item: ${item.getClass}")
        List.empty
    }

  private def buildMany(products: Seq[Any]): Seq[Node[Positioned]] =
    products
      .collect(positionedProducts)
      .flatten

  private def positionedProducts: PartialFunction[Any, Seq[Node[Positioned]]] = {
    case positioned: Positioned =>
      val children = buildOne(positioned)
      List(Node(positioned, children))

    case Some(positioned: Positioned) =>
      val children = buildOne(positioned)
      List(Node(positioned, children))

    case positions: Seq[_] =>
      buildMany(positions)

    case Some(positions: Seq[_]) =>
      buildMany(positions)
  }
}
