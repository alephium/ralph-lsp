// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.AstExtra
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

private object GoToDefTypeId extends StrictImplicitLogging {

  /**
   * Navigate to the positioned ASTs for the given type identifier.
   *
   * @param typeIdNode  The node representing the type identifier in the AST.
   * @param sourceCode The parsed state of the source-code where the search was executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An array sequence of positioned ASTs matching the search result.
   */
  def goTo(
      typeIdNode: Node[Ast.TypeId, Ast.Positioned],
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    typeIdNode.parent match { // take one step up to check the type of TypeId node.
      case Some(parent) =>
        parent match {
          case Node(enumFieldSelector: Ast.EnumFieldSelector[_], _) if enumFieldSelector.enumId == typeIdNode.data =>
            // They selected an enum type. Take 'em there!
            goToEnumType(
              enumFieldSelector = enumFieldSelector,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(emitEvent: Ast.EmitEvent[_], _) if emitEvent.id == typeIdNode.data =>
            // They selected an event emit. Take 'em there!
            WorkspaceSearcher
              .collectInheritedParents(sourceCode, workspace)
              .parentTrees
              .iterator
              .flatMap(goToEventDef(emitEvent, _))

          case Node(enumDef: Ast.EnumDef[_], _) if enumDef.id == typeIdNode.data =>
            Iterator(
              SourceLocation.NodeStrict(
                ast = enumDef.id,
                source = sourceCode
              )
            )

          case Node(eventDef: Ast.EventDef, _) if eventDef.id == typeIdNode.data =>
            Iterator(
              SourceLocation.NodeStrict(
                ast = eventDef.id,
                source = sourceCode
              )
            )

          case Node(globalDef: Ast.GlobalDefinition, _) if AstExtra.getTypeId(globalDef) contains typeIdNode.data =>
            val id =
              AstExtra
                .getIdentOrTypeId(globalDef)
                .merge

            Iterator(
              SourceLocation.NodeStrict(
                ast = id,
                source = sourceCode
              )
            )

          case _ =>
            // For everything else find Contracts, Interfaces, or TxScripts with the given type ID.
            goToCodeId(
              typeId = typeIdNode.data,
              workspace = workspace
            )
        }

      case None =>
        logger.error(s"Parent node not found for AST '${typeIdNode.data.getClass.getSimpleName}' at source index '${typeIdNode.data.sourceIndex}'")
        Iterator.empty
    }

  /**
   * Navigate to the enum types for the given enum field selector.
   *
   * @param enumFieldSelector The enum type to find.
   * @param sourceCode        The parsed state of the source-code where the search was executed.
   * @param workspace         The workspace where this search was executed and where all the source trees exist.
   * @return An array sequence of enum [[Ast.TypeId]]s matching the enum type.
   */
  private def goToEnumType(
      enumFieldSelector: Ast.EnumFieldSelector[_],
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] = {
    val trees =
      WorkspaceSearcher.collectInheritedParents(
        sourceCode = sourceCode,
        workspace = workspace
      )

    // Enums might be within the scope of inheritance, i.e. within a inherited parent Contract
    val parents =
      trees.parentTrees

    // or they might be global enum types
    val globalEnums =
      SourceCodeSearcher.collectGlobalEnumsCode(trees.allTrees.iterator)

    // collected all trees that are in scope for an enum type access
    val allTrees =
      parents ++ globalEnums

    // find matching enum TypeId within each of those trees
    allTrees
      .iterator
      .flatMap(goToEnumType(enumFieldSelector, _))
  }

  /**
   * Navigate to the enum types for the given enum field selector.
   *
   * @param enumSelector The enum type to find.
   * @param source       The source tree to search within.
   * @return An array sequence of enum [[Ast.TypeId]]s matching the enum type.
   */
  private def goToEnumType(
      enumSelector: Ast.EnumFieldSelector[_],
      source: SourceLocation.CodeStrict): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    source.tree.ast match {
      case contract: Ast.Contract =>
        contract
          .enums
          .iterator
          .filter(_.id == enumSelector.enumId)
          .map {
            enumDef =>
              SourceLocation.NodeStrict(enumDef.id, source)
          }

      case enumDef: Ast.EnumDef[_] =>
        if (enumDef.id == enumSelector.enumId)
          Iterator(SourceLocation.NodeStrict(enumDef.id, source))
        else
          Iterator.empty

      case _: Ast.ContractInterface | _: Ast.TxScript | _: Ast.Struct | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
        Iterator.empty
    }

  /**
   * Navigate to the event definition.
   *
   * @param emitEvent The event definition contain the event type identifier to find calls for.
   * @param source    The source tree to search within.
   * @return An array sequence of event definitions [[Ast.EventDef]]s matching the search result.
   */
  private def goToEventDef(
      emitEvent: Ast.EmitEvent[_],
      source: SourceLocation.CodeStrict): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    source
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(eventDef: Ast.EventDef, _) if eventDef.id == emitEvent.id =>
          SourceLocation.NodeStrict(eventDef.id, source)
      }

  /**
   * Navigate to Contracts, Interfaces, or TxScripts by their type ID.
   *
   * @param typeId    The type ID to locate.
   * @param workspace The Workspace where the implementation of the type ID may exist.
   * @return An iterator over search result locations.
   */
  private def goToCodeId(
      typeId: Ast.TypeId,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    WorkspaceSearcher
      .collectTypes(workspace, includeNonImportedCode = false)
      .filter(_.ast == typeId)

}
