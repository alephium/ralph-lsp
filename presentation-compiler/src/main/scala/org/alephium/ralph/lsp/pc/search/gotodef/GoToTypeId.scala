package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node

import scala.collection.immutable.ArraySeq

private object GoToTypeId {

  /**
   * Navigate to the positioned ASTs for the given type identifier.
   *
   * @param identNode The node representing the type identifier in the AST.
   * @param typeId    The type identifier for which the [[Ast.TypeId]] is sought.
   * @param source    The source tree to search within.
   * @return An array sequence of positioned ASTs matching the search result.
   * */
  def goTo(identNode: Node[Positioned],
           typeId: Ast.TypeId,
           source: Tree.Source): ArraySeq[Ast.Positioned] =
    identNode
      .parent // take one step up to check the type of TypeId node.
      .map(_.data)
      .to(ArraySeq)
      .collect {
        case enumFieldSelector: Ast.EnumFieldSelector[_] if enumFieldSelector.enumId == typeId =>
          // The user clicked on an enum type. Take 'em there!
          goToEnumType(
            enumSelector = enumFieldSelector,
            source = source
          )

        case enumDef: Ast.EnumDef if enumDef.id == typeId =>
          // The user clicked on an enum definition. Take 'em there!
          goToEnumDefCalls(
            enumDef = enumDef,
            source = source
          )
      }
      .flatten

  /** Navigate to the enum types for the given enum field selector.
   *
   * @param enumSelector The enum type to find.
   * @param source       The source tree to search within.
   * @return An array sequence of enum [[Ast.TypeId]]s matching the search result.
   * */
  private def goToEnumType(enumSelector: Ast.EnumFieldSelector[_],
                           source: Tree.Source): ArraySeq[Ast.TypeId] =
    source.ast match {
      case contract: Ast.Contract =>
        contract
          .enums
          .filter(_.id == enumSelector.enumId)
          .map(_.id)
          .to(ArraySeq)

      case _: Ast.ContractInterface | _: Ast.TxScript =>
        ArraySeq.empty
    }

  /** Navigate to the enum type name calls.
   *
   * @param enumDef The enum definition to find calls for.
   * @param source  The source tree to search within.
   * @return An array sequence of enum type [[Ast.TypeId]]s matching the search result.
   * */
  private def goToEnumDefCalls(enumDef: Ast.EnumDef,
                               source: Tree.Source): ArraySeq[Ast.TypeId] =
    source
      .rootNode
      .walkDown
      .collect {
        case Node(selector: Ast.EnumFieldSelector[_], _) if selector.enumId == enumDef.id =>
          selector.enumId
      }
      .to(ArraySeq)

}
