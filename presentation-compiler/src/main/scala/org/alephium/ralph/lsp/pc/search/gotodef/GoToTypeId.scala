package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceTreeInScope
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

private object GoToTypeId {

  /**
   * Navigate to the positioned ASTs for the given type identifier.
   *
   * @param identNode  The node representing the type identifier in the AST.
   * @param typeId     The type identifier for which the [[Ast.TypeId]] is sought.
   * @param sourceCode The parsed state of the source-code where the search was executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An array sequence of positioned ASTs matching the search result.
   * */
  def goTo(identNode: Node[Positioned],
           typeId: Ast.TypeId,
           sourceCode: SourceTreeInScope,
           workspace: WorkspaceState.IsSourceAware): Iterator[GoToLocation] =
    identNode.parent match { // take one step up to check the type of TypeId node.
      case Some(parent) =>
        parent match {
          case Node(enumFieldSelector: Ast.EnumFieldSelector[_], _) if enumFieldSelector.enumId == typeId =>
            // They selected an enum type. Take 'em there!
            GoTo.inScope(
              sourceCode = sourceCode,
              workspace = workspace,
              searcher = goToEnumType(enumFieldSelector, _)
            )

          case Node(enumDef: Ast.EnumDef, _) if enumDef.id == typeId =>
            // They selected an enum definition. Find enum usages.
            goToEnumTypeUsage(
              enumDef = enumDef,
              source = sourceCode.tree
            ).flatMap(GoToLocation(_, sourceCode.parsed))

          case Node(emitEvent: Ast.EmitEvent[_], _) if emitEvent.id == typeId =>
            // They selected an event emit. Take 'em there!
            goToEventDef(
              emitEvent = emitEvent,
              source = sourceCode.tree
            ).flatMap(GoToLocation(_, sourceCode.parsed))

          case Node(eventDef: Ast.EventDef, _) if eventDef.id == typeId =>
            // They selected an event definition. Find event usages.
            goToEventDefUsage(
              eventDef = eventDef,
              source = sourceCode.tree
            ).flatMap(GoToLocation(_, sourceCode.parsed))

          case _ =>
            Iterator.empty
        }

      case None =>
        Iterator.empty
    }

  /** Navigate to the enum types for the given enum field selector.
   *
   * @param enumSelector The enum type to find.
   * @param source       The source tree to search within.
   * @return An array sequence of enum [[Ast.TypeId]]s matching the search result.
   * */
  private def goToEnumType(enumSelector: Ast.EnumFieldSelector[_],
                           source: Tree.Source): Iterator[Ast.TypeId] =
    source.ast match {
      case Left(contract: Ast.Contract) =>
        contract
          .enums
          .iterator
          .filter(_.id == enumSelector.enumId)
          .map(_.id)

      case Left(_: Ast.ContractInterface | _: Ast.TxScript) | Right(_: Ast.Struct) =>
        Iterator.empty
    }

  /** Navigate to the enum type name usage.
   *
   * @param enumDef The enum definition containing the enum type identifier to find calls for.
   * @param source  The source tree to search within.
   * @return An array sequence of enum type [[Ast.TypeId]]s matching the search result.
   * */
  private def goToEnumTypeUsage(enumDef: Ast.EnumDef,
                                source: Tree.Source): Iterator[Ast.TypeId] =
    source
      .rootNode
      .walkDown
      .collect {
        case Node(selector: Ast.EnumFieldSelector[_], _) if selector.enumId == enumDef.id =>
          selector.enumId
      }

  /**
   * Navigate to the event definition.
   *
   * @param emitEvent The event definition contain the event type identifier to find calls for.
   * @param source    The source tree to search within.
   * @return An array sequence of event definitions [[Ast.EventDef]]s matching the search result.
   */
  private def goToEventDef(emitEvent: Ast.EmitEvent[_],
                           source: Tree.Source): Iterator[Ast.EventDef] =
    source
      .rootNode
      .walkDown
      .collect {
        case Node(eventDef: Ast.EventDef, _) if eventDef.id == emitEvent.id =>
          eventDef
      }

  /** Navigate to the event type name usages.
   *
   * @param eventDef The event definition containing the enum type identifier to find calls for.
   * @param source   The source tree to search within.
   * @return An array sequence of enum type [[Ast.TypeId]]s matching the search result.
   * */
  private def goToEventDefUsage(eventDef: Ast.EventDef,
                                source: Tree.Source): Iterator[Ast.TypeId] =
    source
      .rootNode
      .walkDown
      .collect {
        case Node(emitEvent: Ast.EmitEvent[_], _) if emitEvent.id == eventDef.id =>
          emitEvent.id
      }

}
