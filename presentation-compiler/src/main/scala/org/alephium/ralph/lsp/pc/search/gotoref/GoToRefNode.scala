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

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.{AstExtra, Tree}
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, ImplementingChildrenResult, WorkspaceSearcher}

import scala.collection.immutable.ArraySeq

object GoToRefNode {

  /**
   * Navigates to the definition of a token in the source code.
   *
   * @param cursorIndex The index of the token clicked by the user.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param workspace   The workspace where this search was executed and where all the source trees exist.
   * @return An iterator over the target go-to location(s).
   */
  def goTo(
      definition: Node[Ast.Positioned, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware,
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.Node[Ast.Positioned]] =
    definition match {
      case node @ Node(field: Ast.EnumField[_], _) =>
        // They selected an enum field.
        val result =
          goToEnumFieldUsage(
            node = node.upcast(field),
            sourceCode = sourceCode,
            workspace = workspace
          )

        addDefinition(
          definitionAST = field.ident,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case node @ Node(field: Ast.EventField, _) =>
        // They selected an event field.
        // Check the parent to find the event definition.
        val result =
          goToEventFieldUsages(
            node = node.upcast(field),
            sourceCode = sourceCode,
            workspace = workspace
          )

        addDefinition(
          definitionAST = field.ident,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case Node(constantDef: Ast.ConstantVarDef[_], _) =>
        val result =
          goToConstantUsage(
            constantDef = constantDef,
            sourceCode = sourceCode,
            workspace = workspace
          )

        addDefinition(
          definitionAST = constantDef.ident,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case namedVarNode @ Node(namedVar: Ast.NamedVar, _) =>
        // User selected a named variable. Find its usages.
        val result =
          goToLocalVariableUsage(
            fromNode = namedVarNode.upcast(namedVar),
            sourceCode = sourceCode
          )

        addDefinition(
          definitionAST = namedVar.ident,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case argumentNode @ Node(argument: Ast.Argument, _) =>
        // They selected an argument. Take 'em there!
        val result =
          goToArgumentUsage(
            argumentNode = argumentNode.upcast(argument),
            sourceCode = sourceCode,
            workspace = workspace
          )

        addDefinition(
          definitionAST = argument.ident,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case Node(mapFuncCall: Ast.MapDef, _) =>
        // Go-to MapDef usages.
        val result =
          goToMapDefUsages(
            ident = mapFuncCall.ident,
            sourceCode = sourceCode,
            workspace = workspace
          )

        addDefinition(
          definitionAST = mapFuncCall.ident,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case Node(funcDef: Ast.FuncDef[_], _) =>
        val result =
          goToFunctionUsage(
            funcId = funcDef.id,
            sourceCode = sourceCode,
            workspace = workspace
          )

        addDefinition(
          definitionAST = funcDef.id,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case Node(enumDef: Ast.EnumDef[_], _) =>
        // They selected an enum definition. Find enum usages.
        val result =
          goToEnumDefUsage(
            enumDef = enumDef,
            sourceCode = sourceCode,
            workspace = workspace
          )

        addDefinition(
          definitionAST = enumDef.id,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case Node(eventDef: Ast.EventDef, _) =>
        // They selected an event definition. Find event usages.
        val result =
          goToEventDefUsage(
            eventDef = eventDef,
            sourceCode = sourceCode,
            workspace = workspace
          )

        addDefinition(
          definitionAST = eventDef.id,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case Node(globalDef: Ast.GlobalDefinition, _) =>
        val result =
          goToTypeIdUsage(
            globalDef = globalDef,
            workspace = workspace
          )

        AstExtra.getTypeId(globalDef) match {
          case Some(typeId) =>
            addDefinition(
              definitionAST = typeId,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = isIncludeDeclaration
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

        addDefinition(
          definitionAST = typeId,
          definitionSource = sourceCode,
          result = result,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case _ =>
        Iterator.empty
    }

  private def goToEventFieldUsages(
      node: Node[Ast.EventField, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware) =
    // They selected an event field.
    // Check the parent to find the event definition.
    node
      .parent
      .map(_.data)
      .iterator
      .collect {
        // Check: Parent is an event definition which contains the event field.
        case eventDef: Ast.EventDef if eventDef.fields.exists(_.ident == node.data.ident) =>
          WorkspaceSearcher
            .collectImplementingChildren(sourceCode, workspace)
            .childTrees
            .iterator
            .flatMap {
              sourceCode =>
                goToEventFieldUsage(
                  eventDefId = eventDef.id,
                  eventFieldIndex = eventDef.fields.indexWhere(_.ident == node.data.ident),
                  sourceCode = sourceCode
                )
            }
      }
      .flatten

  /**
   * Navigate to enum field usages.
   *
   * @param node       The node representing the enum field being searched.
   * @param sourceCode The source tree where this search was executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An iterator containing identities representing the usage locations of the enum field.
   */
  private def goToEnumFieldUsage(
      node: Node[Ast.EnumField[_], Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Ident]] =
    // Check the parent to find the enum type.
    node
      .parent
      .map(_.data)
      .iterator
      .collect {
        // Check: Parent is an enum definition which contains the enum field.
        case enumDef: Ast.EnumDef[_] if enumDef.fields.exists(_.ident == node.data.ident) =>
          val trees =
            if (sourceCode.tree.ast == enumDef)
              WorkspaceSearcher.collectAllTrees(workspace) // It's a global enum, search all workspace trees.
            else
              WorkspaceSearcher // It's a local enum, search within the scope of inheritance.
                .collectImplementingChildren(sourceCode, workspace)
                .childTrees

          trees
            .iterator
            .flatMap {
              sourceCode =>
                goToEnumFieldUsage(
                  enumType = enumDef.id,
                  enumField = node.data,
                  sourceCode = sourceCode
                )
            }
      }
      .flatten

  /**
   * Navigate to variable usages of the given identity.
   *
   * @param fromNode   The node representing the identity being searched.
   * @param sourceCode The source tree to search within.
   * @return An iterator containing identities representing the usage locations of input identity.
   */
  private def goToLocalVariableUsage(
      fromNode: Node[Ast.NamedVar, Ast.Positioned],
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Ident]] =
    goToNearestBlockInScope(fromNode, sourceCode.tree)
      .iterator
      .flatMap {
        from =>
          goToVariableUsages(
            ident = fromNode.data.ident,
            from = from,
            sourceCode = sourceCode
          )
      }

  /**
   * Navigate to argument usages.
   *
   * @param argumentNode The node representing the selected argument.
   * @param sourceCode   The source where this argument exists.
   * @param workspace    The workspace that may contain other dependant source-code.
   * @return An iterator containing identities representing the usage locations of the argument.
   */
  private def goToArgumentUsage(
      argumentNode: Node[Ast.Argument, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Ident]] =
    goToNearestFuncDef(argumentNode) match {
      case Some(functionNode) =>
        // It's a function argument, search within this function's body.
        goToVariableUsages(
          ident = argumentNode.data.ident,
          from = functionNode,
          sourceCode = sourceCode
        )

      case None =>
        // It's a template argument, search within the source-tree and within all dependant code.
        goToTemplateArgumentUsage(
          argument = argumentNode.data,
          sourceCode = sourceCode,
          workspace = workspace
        )
    }

  /**
   * Navigate to the usages of a template argument.
   *
   * @param argument   The template argument whose usages are to be searched.
   * @param sourceCode The source where this argument exists.
   * @param workspace  The workspace that may contain other dependant source-code.
   * @return An iterator containing identities representing the usage locations of the template argument.
   */
  private def goToTemplateArgumentUsage(
      argument: Ast.Argument,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Ident]] = {
    val contractInheritanceUsage =
      goToArgumentsUsageInInheritanceDefinition(
        argument = argument,
        sourceCode = sourceCode
      )

    val withInheritanceUsage =
      goToTemplateArgumentUsageWithinInheritance(
        argument = argument,
        sourceCode = sourceCode,
        workspace = workspace
      )

    contractInheritanceUsage ++ withInheritanceUsage
  }

  /**
   * Navigate to the usages of a template argument within the scope when inheritance is defined.
   *
   * For example:
   * {{{
   *   Contract MyContract(boolean: Bool) extends OtherContract(>>boolean<<) // within OtherContract only
   * }}}
   *
   * @param argument   The template argument whose usages are to be searched.
   * @param sourceCode The source where this argument exists.
   * @return An iterator containing identities representing the usage locations of the template argument.
   */
  private def goToArgumentsUsageInInheritanceDefinition(
      argument: Ast.Argument,
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Ident]] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(ast: Ast.ContractInheritance, _) =>
          ast
            .idents
            .filter(_ == argument.ident)
            .map {
              ident =>
                SourceLocation.Node(ident, sourceCode)
            }
      }
      .flatten

  /**
   * Navigate to the usages of a template argument within the scope of inheriting contracts.
   *
   * @param argument   The template argument whose usages are to be searched.
   * @param sourceCode The source where this argument exists.
   * @param workspace  The workspace that may contain other dependant source-code.
   * @return An iterator containing identities representing the usage locations of the template argument.
   */
  private def goToTemplateArgumentUsageWithinInheritance(
      argument: Ast.Argument,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Ident]] =
    WorkspaceSearcher
      .collectImplementingChildren(sourceCode, workspace)
      .childTrees
      .iterator
      .flatMap {
        sourceCode =>
          goToVariableUsages(
            ident = argument.ident,
            from = sourceCode.tree.rootNode,
            sourceCode = sourceCode
          )
      }

  /**
   * Navigate to all enum usages for the given enum type and field.
   *
   * @param enumType   The enum type to find.
   * @param enumField  The enum field to find.
   * @param sourceCode The source tree to search within.
   * @return An iterator over used/accessed enum field identities.
   */
  private def goToEnumFieldUsage(
      enumType: Ast.TypeId,
      enumField: Ast.EnumField[_],
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Ident]] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .collect {
        // find all the selections matching the enum and the enum's field type.
        case Node(selector: Ast.EnumFieldSelector[_], _) if selector.enumId == enumType && selector.field == enumField.ident =>
          SourceLocation.Node(selector.field, sourceCode)
      }

  /**
   * Navigate to all event field usages for an event at the index of the event field.
   *
   * @param eventDefId      The event definition ID to find.
   * @param eventFieldIndex The index of the event field.
   * @param sourceCode      The source tree to search within.
   * @return An iterator over expressions defined in position of the event field.
   */
  private def goToEventFieldUsage(
      eventDefId: Ast.TypeId,
      eventFieldIndex: Int,
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Expr[_]]] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .collect {
        // find all the event fields usages at given eventFieldIndex.
        case Node(emitEvent: Ast.EmitEvent[_], _) if emitEvent.id == eventDefId && eventFieldIndex < emitEvent.args.length =>
          SourceLocation.Node(emitEvent.args(eventFieldIndex), sourceCode)
      }

  /**
   * Navigate to all variable usages for the given variable identifier.
   *
   * @param ident The variable identifier to search for.
   * @param from  The node to search within, walking downwards.
   * @return An array sequence of variable usage IDs.
   */
  private def goToVariableUsages(
      ident: Ast.Ident,
      from: Node[Ast.Positioned, Ast.Positioned],
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Ident]] =
    from
      .walkDown
      .collect {
        // find all the selections matching the variable name.
        case Node(variable: Ast.Variable[_], _) if variable.id == ident =>
          SourceLocation.Node(variable.id, sourceCode)

        // collect all assignments
        case Node(variable: Ast.AssignmentTarget[_], _) if variable.ident == ident =>
          SourceLocation.Node(variable.ident, sourceCode)
      }

  /**
   * Navigate to the nearest code block for which the given child node is in scope.
   *
   * @param childNode The node within the scoped block.
   * @return An Option containing the nearest parent block, if found.
   */
  private def goToNearestBlockInScope(
      childNode: Node[Ast.Positioned, Ast.Positioned],
      sourceTree: Tree.Source): Option[Node[Ast.UniqueDef, Ast.Positioned]] =
    sourceTree.ast match {
      case _: Ast.Contract | _: Ast.ContractInterface | _: Ast.TxScript =>
        // Find the nearest function definition or use the template body as the scope.
        goToNearestFuncDef(childNode) orElse Some(sourceTree.rootNode)

      case _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
        None
    }

  /**
   * Navigates to the usages of map definition(s) with the given identifier.
   *
   * @param ident      The identifier of the map definition to find usages for.
   * @param sourceCode The source tree where this search is executed.
   * @param workspace  The workspace where this search is executed and where all source trees exist.
   * @return An iterator over [[Ast.Positioned]] representing the usage locations.
   */
  private def goToMapDefUsages(
      ident: Ast.Ident,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Positioned]] =
    // Go-to MapDef usages.
    WorkspaceSearcher
      .collectImplementingChildren(sourceCode, workspace)
      .childTrees
      .iterator
      .flatMap {
        code =>
          code.tree.rootNode.walkDown.collect {
            case Node(mapCall: Ast.MapFuncCall, _) if mapCall.ident == ident =>
              SourceLocation.Node(mapCall.ident, code)

            case Node(mapCall: Ast.MapContains, _) if mapCall.ident == ident =>
              SourceLocation.Node(mapCall.ident, code)

            case Node(Ast.LoadDataBySelectors(Ast.Variable(variableIdent), _), _) if variableIdent == ident =>
              SourceLocation.Node(variableIdent, code)

            case Node(mapCall: Ast.AssignmentSelectedTarget[_], _) if mapCall.ident == ident =>
              SourceLocation.Node(mapCall.ident, code)
          }
      }

  /**
   * Navigate to local and global constant usages.
   *
   * @param constantDef The constant definition to search usages for.
   * @param sourceCode  The source tree where this search is executed.
   * @param workspace   The workspace where this search is executed and where all source trees exist.
   * @return An iterator over identity nodes representing the constant usage locations.
   */
  private def goToConstantUsage(
      constantDef: Ast.ConstantVarDef[_],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Ident]] = {
    val children =
      if (sourceCode.tree.ast == constantDef)
        // Is a global constant, fetch all workspace trees.
        WorkspaceSearcher.collectAllTrees(workspace)
      else
        // Is a local constant, fetch all trees within the scope of inheritance.
        WorkspaceSearcher
          .collectImplementingChildren(sourceCode, workspace)
          .childTrees

    children
      .iterator
      .flatMap {
        sourceCode =>
          goToVariableUsages(
            ident = constantDef.ident,
            from = sourceCode.tree.rootNode,
            sourceCode = sourceCode
          )
      }
  }

  /**
   * Navigate to the nearest function definition for which the given child node is in scope.
   *
   * @param childNode The node to traverse up from.
   * @return An Option containing the nearest function definition, if found.
   */
  def goToNearestFuncDef(childNode: Node[Ast.Positioned, Ast.Positioned]): Option[Node[Ast.FuncDef[_], Ast.Positioned]] =
    childNode.data match {
      case function: Ast.FuncDef[_] =>
        // Nested function definitions are not allowed in Ralph.
        // If the input node is a function, return the node itself.
        Some(childNode.upcast(function))

      case ast: Ast.Positioned =>
        // For everything else, find the nearest function.
        ast
          .sourceIndex
          .flatMap {
            childNodeIndex =>
              childNode
                .walkParents
                .collectFirst {
                  case node @ Node(function: Ast.FuncDef[_], _) if function.sourceIndex.exists(_ contains childNodeIndex.index) =>
                    node.upcast(function)
                }
          }
    }

  /**
   * Navigate to all function usages within the source code for the specified [[Ast.FuncId]].
   *
   * @param funcId     The [[Ast.FuncId]] of the function to locate.
   * @param sourceCode The source tree to search.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An iterator over all searched function usages.
   */
  private def goToFunctionUsage(
      funcId: Ast.FuncId,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Positioned]] =
    if (funcId.isBuiltIn) {
      val allTrees =
        WorkspaceSearcher.collectAllTrees(workspace)

      goToDirectCallFunctionUsage(
        funcId = funcId,
        children = allTrees
      )
    } else {
      val children =
        WorkspaceSearcher.collectImplementingChildren(
          sourceCode = sourceCode,
          workspace = workspace
        )

      // Direct calls can only occur within the scope of inheritance.
      val directCallUsages =
        goToDirectCallFunctionUsage(
          funcId = funcId,
          children = children.childTrees
        )

      // Contract call can occur anywhere where an instance of the Contract can be created.
      val contractCallUsages =
        goToContractCallFunctionUsage(
          funcId = funcId,
          children = children
        )

      directCallUsages ++ contractCallUsages
    }

  /**
   * Navigates to all direct function call usages for the specified [[Ast.FuncId]].
   *
   * @param funcId   The [[Ast.FuncId]] of the function to locate usages for.
   * @param children The sources to search within.
   * @return An iterator over all found function call usages.
   */
  private def goToDirectCallFunctionUsage(
      funcId: Ast.FuncId,
      children: ArraySeq[SourceLocation.Code]): Iterator[SourceLocation.Node[Ast.Positioned]] =
    children
      .iterator
      .flatMap {
        sourceCode =>
          goToFunctionUsage(
            funcId = funcId,
            sourceCode = sourceCode
          )
      }

  /**
   * Navigates to all contract call usages for the specified [[Ast.FuncId]].
   *
   * @param funcId   The [[Ast.FuncId]] of the function to locate contract call usages for.
   * @param children The result containing the child trees and all trees in scope within the current workspace.
   * @return An iterator over all found contract call usages.
   */
  private def goToContractCallFunctionUsage(
      funcId: Ast.FuncId,
      children: ImplementingChildrenResult): Iterator[SourceLocation.Node[Ast.Positioned]] =
    children
      .allTrees // traverse all trees to find contract call usages, eg: `myContract.function()` or `MyContract().function()`
      .iterator
      .flatMap {
        code =>
          code.tree.rootNode.walkDown.collect {
            case Node(call: Ast.ContractCallBase, _) if call.callId == funcId =>
              // function ID matches, but does it also match the type ID?
              call
                .obj
                .getCachedType()
                .map(_.flatMap(AstExtra.getTypeId)) // fetch the type ID of this function call
                .iterator
                .flatMap {
                  thisCallTypes =>
                    children
                      .childTrees // at least one of the child trees should match this call's object type
                      .flatMap {
                        childCode =>
                          if (childCode.tree.typeId() exists thisCallTypes.contains)
                            Some(SourceLocation.Node(call.callId, code)) // Matched! Both the `funcId` and `typeId` are a match.
                          else
                            None
                      }
                }
          }
      }
      .flatten

  /**
   * Navigate to all local function usage where the given function definition [[Ast.FuncDef]]
   * is invoked.
   *
   * @param funcId     The [[Ast.FuncId]] of the [[Ast.FuncDef]] to locate calls for.
   * @param sourceCode The source tree to search within.
   * @return An iterator containing all the local function calls.
   */
  private def goToFunctionUsage(
      funcId: Ast.FuncId,
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Positioned]] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(exp: Ast.CallExpr[_], _) if exp.id == funcId =>
          SourceLocation.Node(exp.id, sourceCode)

        case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcId =>
          SourceLocation.Node(funcCall.id, sourceCode)
      }

  /**
   * Navigate to the enum definition usages.
   *
   * @param enumDef    The enum definition to find usages for.
   * @param sourceCode The parsed state of the source-code where the search was executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An array sequence of enum [[Ast.TypeId]]s matching the enum definition.
   */
  def goToEnumDefUsage(
      enumDef: Ast.EnumDef[_],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.TypeId]] = {
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
      source: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.TypeId]] =
    source
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(selector: Ast.EnumFieldSelector[_], _) if selector.enumId == enumDef.id =>
          SourceLocation.Node(selector.enumId, source)
      }

  private def goToEventDefUsage(
      eventDef: Ast.EventDef,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.TypeId]] =
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
      source: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.TypeId]] =
    source
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(emitEvent: Ast.EmitEvent[_], _) if emitEvent.id == eventDef.id =>
          SourceLocation.Node(emitEvent.id, source)
      }

  private def goToTypeIdUsage(
      globalDef: Ast.GlobalDefinition,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.TypeId]] =
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
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.TypeId]] =
    WorkspaceSearcher
      .collectAllTrees(workspace)
      .iterator
      .flatMap {
        code =>
          code.tree.rootNode.walkDown.collect {
            // this typeId should equal the searched typeId and a different object then itself.
            case Node(typeId: Ast.TypeId, _) if typeId == selectedTypId && typeId.ne(selectedTypId) =>
              SourceLocation.Node(
                ast = typeId,
                source = code
              )
          }
      }

  private def addDefinition(
      definitionAST: Ast.TypeId,
      definitionSource: SourceLocation.Code,
      result: Iterator[SourceLocation.Node[Ast.Positioned]],
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.Node[Ast.Positioned]] =
    __addIDDefinition(
      definitionAST = definitionAST,
      definitionSource = definitionSource,
      result = result,
      isIncludeDeclaration = isIncludeDeclaration
    )

  private def addDefinition(
      definitionAST: Ast.Ident,
      definitionSource: SourceLocation.Code,
      result: Iterator[SourceLocation.Node[Ast.Positioned]],
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.Node[Ast.Positioned]] =
    __addIDDefinition(
      definitionAST = definitionAST,
      definitionSource = definitionSource,
      result = result,
      isIncludeDeclaration = isIncludeDeclaration
    )

  private def addDefinition(
      definitionAST: Ast.FuncId,
      definitionSource: SourceLocation.Code,
      result: Iterator[SourceLocation.Node[Ast.Positioned]],
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.Node[Ast.Positioned]] =
    __addIDDefinition(
      definitionAST = definitionAST,
      definitionSource = definitionSource,
      result = result,
      isIncludeDeclaration = isIncludeDeclaration
    )

  /**
   * DO NOT CALL THIS FUNCTION DIRECTLY. Use one of the above instead.
   *
   * This function is needed because the AST ID types ([[Ast.TypeId]], [[Ast.Ident]], [[Ast.FuncId]])
   * do not have a common subtype. And only these three types should be added to the result when
   * `isIncludeDeclaration` is `true`.
   */
  @inline private def __addIDDefinition[A <: Ast.Positioned](
      definitionAST: A,
      definitionSource: SourceLocation.Code,
      result: Iterator[SourceLocation.Node[Ast.Positioned]],
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.Node[Ast.Positioned]] =
    if (isIncludeDeclaration) {
      val definition =
        SourceLocation.Node(
          ast = definitionAST,
          source = definitionSource
        )

      Iterator.single(definition) ++ result
    } else {
      result
    }

}
