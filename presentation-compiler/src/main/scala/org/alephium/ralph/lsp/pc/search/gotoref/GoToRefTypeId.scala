// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.AstExtra
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

private object GoToRefTypeId extends StrictImplicitLogging {

  /**
   * Navigates to the references of a token in the source code.
   *
   * @param definition The definition to search references for.
   * @param sourceCode The parsed state of the source-code where the search is executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @param settings   Search settings.
   * @return An iterator over the target go-to location(s).
   */
  def goTo(
      definition: Node[Ast.TypeId, Ast.Positioned],
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToRefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    definition.parent match {
      case Some(parent) =>
        parent match {
          case Node(enumDef: Ast.EnumDef[_], _) =>
            // They selected an enum definition. Find enum usages.
            val result =
              goToEnumDefUsage(
                enumDef = enumDef,
                sourceCode = sourceCode,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = enumDef.id,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case Node(eventDef: Ast.EventDef, _) =>
            // They selected an event definition. Find event usages.
            val result =
              goToEventDefUsage(
                eventDef = eventDef,
                sourceCode = sourceCode,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = eventDef.id,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case Node(globalDef: Ast.GlobalDefinition, _) =>
            // Ast.GlobalDefinition is never expected to be submitted here,
            // because they always have an associated type-id.
            // But this is processed temporarily, just in-case.
            // The behaviour here is the same as processing [[Ast.TypeId]].
            val result =
              goToTypeIdUsage(
                globalDef = globalDef,
                workspace = workspace
              )

            AstExtra.getTypeId(globalDef) match {
              case Some(typeId) =>
                IncludeDeclaration.add(
                  definitionAST = typeId,
                  definitionSource = sourceCode,
                  result = result,
                  isIncludeDeclaration = settings.includeDeclaration
                )

              case None =>
                result
            }

          case Node(typeId: Ast.TypeId, _) =>
            val result =
              goToTypeIdUsage(
                selectedTypId = typeId,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = typeId,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case Node(ast, _) =>
            logger.error(s"No GoToRef implementation for '${ast.getClass.getSimpleName}'")
            Iterator.empty
        }

      case None =>
        logger.error(s"Parent node not found for AST '${definition.data.getClass.getSimpleName}' at source index '${definition.data.sourceIndex}'")
        Iterator.empty
    }

  /**
   * Navigate to the enum definition usages.
   *
   * @param enumDef    The enum definition to find usages for.
   * @param sourceCode The parsed state of the source-code where the search was executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An array sequence of enum [[Ast.TypeId]]s matching the enum definition.
   */
  private def goToEnumDefUsage(
      enumDef: Ast.EnumDef[_],
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] = {
    val trees =
      if (sourceCode.tree.ast == enumDef)
        WorkspaceSearcher.collectAllTrees(workspace) // It's a global enum, search all workspace trees
      else
        WorkspaceSearcher // It's a local enum, search only the trees within the scope of inheritance
          .collectImplementingChildren(sourceCode, workspace)
          .childTrees

    trees
      .iterator
      .flatMap(goToEnumTypeUsage(enumDef, _))
  }

  /**
   * Navigate to the enum type name usage.
   *
   * @param enumDef The enum definition containing the enum type identifier to find calls for.
   * @param source  The source tree to search within.
   * @return An array sequence of enum type [[Ast.TypeId]]s matching the search result.
   */
  private def goToEnumTypeUsage(
      enumDef: Ast.EnumDef[_],
      source: SourceLocation.CodeStrict): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    source
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(selector: Ast.EnumFieldSelector[_], _) if selector.enumId == enumDef.id =>
          SourceLocation.NodeStrict(selector.enumId, source)
      }

  private def goToEventDefUsage(
      eventDef: Ast.EventDef,
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    // They selected an event definition. Find event usages.
    WorkspaceSearcher
      .collectImplementingChildren(sourceCode, workspace)
      .childTrees
      .iterator
      .flatMap(goToEventDefUsage(eventDef, _))

  /**
   * Navigate to the event type name usages.
   *
   * @param eventDef The event definition containing the enum type identifier to find calls for.
   * @param source   The source tree to search within.
   * @return An array sequence of enum type [[Ast.TypeId]]s matching the search result.
   */
  private def goToEventDefUsage(
      eventDef: Ast.EventDef,
      source: SourceLocation.CodeStrict): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    source
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(emitEvent: Ast.EmitEvent[_], _) if emitEvent.id == eventDef.id =>
          SourceLocation.NodeStrict(emitEvent.id, source)
      }

  private def goToTypeIdUsage(
      globalDef: Ast.GlobalDefinition,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    AstExtra.getTypeId(globalDef) match {
      case Some(typeId) =>
        goToTypeIdUsage(
          selectedTypId = typeId,
          workspace = workspace
        )

      case None =>
        Iterator.empty
    }

  /**
   * Navigate to all [[Ast.TypeId]] usages excluding itself.
   *
   * @param selectedTypId The selected typed ID.
   *                      This must be the actual selected [[Ast.TypeId]] instance from the tree.
   * @param workspace     The Workspace where the implementation of the type ID may exist.
   * @return An iterator over [[Ast.TypeId]] usages.
   */
  private def goToTypeIdUsage(
      selectedTypId: Ast.TypeId,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    WorkspaceSearcher
      .collectAllTrees(workspace)
      .iterator
      .flatMap {
        code =>
          code.tree.rootNode.walkDown.collect {
            // this typeId should equal the searched typeId and a different object then itself.
            case Node(typeId: Ast.TypeId, _) if typeId == selectedTypId && typeId.ne(selectedTypId) =>
              SourceLocation.NodeStrict(
                ast = typeId,
                source = code
              )
          }
      }

}
