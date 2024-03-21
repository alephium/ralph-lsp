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
          // They selected an enum type. Take 'em there!
          goToEnumType(
            enumSelector = enumFieldSelector,
            source = source
          )

        case enumDef: Ast.EnumDef if enumDef.id == typeId =>
          // They selected an enum definition. Take 'em there!
          goToEnumTypeUsage(
            enumDef = enumDef,
            source = source
          )

        case emitEvent: Ast.EmitEvent[_] if emitEvent.id == typeId =>
          // They selected an event emit. Take 'em there!
          goToEventDef(
            emitEvent,
            source = source
          )

        case eventDef: Ast.EventDef if eventDef.id == typeId =>
          // They selected an enum definition. Take 'em there!
          goToEventDefUsage(
            eventDef = eventDef,
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
      case Left(contract: Ast.Contract) =>
        contract
          .enums
          .filter(_.id == enumSelector.enumId)
          .map(_.id)
          .to(ArraySeq)

      case Left(_: Ast.ContractInterface | _: Ast.TxScript) | Right(_: Ast.Struct) =>
        ArraySeq.empty
    }

  /** Navigate to the enum type name usage.
   *
   * @param enumDef The enum definition containing the enum type identifier to find calls for.
   * @param source  The source tree to search within.
   * @return An array sequence of enum type [[Ast.TypeId]]s matching the search result.
   * */
  private def goToEnumTypeUsage(enumDef: Ast.EnumDef,
                                source: Tree.Source): ArraySeq[Ast.TypeId] =
    source
      .rootNode
      .walkDown
      .collect {
        case Node(selector: Ast.EnumFieldSelector[_], _) if selector.enumId == enumDef.id =>
          selector.enumId
      }
      .to(ArraySeq)

  /**
   * Navigate to the event definition.
   *
   * @param emitEvent The event definition contain the event type identifier to find calls for.
   * @param source    The source tree to search within.
   * @return An array sequence of event definitions [[Ast.EventDef]]s matching the search result.
   */
  private def goToEventDef(emitEvent: Ast.EmitEvent[_],
                           source: Tree.Source): ArraySeq[Ast.EventDef] =
    source
      .rootNode
      .walkDown
      .collect {
        case Node(eventDef: Ast.EventDef, _) if eventDef.id == emitEvent.id =>
          eventDef
      }
      .to(ArraySeq)

  /** Navigate to the event type name usages.
   *
   * @param eventDef The event definition containing the enum type identifier to find calls for.
   * @param source   The source tree to search within.
   * @return An array sequence of enum type [[Ast.TypeId]]s matching the search result.
   * */
  private def goToEventDefUsage(eventDef: Ast.EventDef,
                                source: Tree.Source): ArraySeq[Ast.TypeId] =
    source
      .rootNode
      .walkDown
      .collect {
        case Node(emitEvent: Ast.EmitEvent[_], _) if emitEvent.id == eventDef.id =>
          emitEvent.id
      }
      .to(ArraySeq)

}
