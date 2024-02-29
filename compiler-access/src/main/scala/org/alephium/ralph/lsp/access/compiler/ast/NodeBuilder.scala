package org.alephium.ralph.lsp.access.compiler.ast

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
  def buildRootNode(ast: Ast.ContractWithState): Node[Positioned] = {
    val fieldsTree = buildMany(ast.fields)
    val eventsTree = buildMany(ast.events)
    val constantVarsTree = buildMany(ast.constantVars)
    val inheritancesTree = buildMany(ast.inheritances)
    val enumsTree = buildMany(ast.enums)
    val templateVarsTree = buildMany(ast.templateVars)
    val funcsTree = buildMany(ast.funcs)
    val eventsInfoTree = buildMany(ast.eventsInfo())
    val builtInContractFuncsTree = buildMany(ast.builtInContractFuncs())

    // TODO: All the above types really siblings? If they are not, they need to build a tree structure using source-index.
    val rootSiblings =
      fieldsTree ++
        eventsTree ++
        constantVarsTree ++
        inheritancesTree ++
        enumsTree ++
        templateVarsTree ++
        funcsTree ++
        eventsInfoTree ++
        builtInContractFuncsTree

    // sort the sibling according to their source-index i.e. following their order of position in code.
    val sortedRootSiblings =
      rootSiblings.sortBy(_.data.sourceIndex.map(_.index))

    // Root node
    Node(
      data = RootPositioned,
      children = sortedRootSiblings
    )
  }

  private def buildOne(positioned: Positioned): List[Node[Positioned]] =
    positioned match {
      case positioned: Product =>
        positioned
          .productIterator
          .toList
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

    case positions: Seq[_] =>
      buildMany(positions)

    case Some(positions: Seq[_]) =>
      buildMany(positions)
  }
}
