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
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefIdent
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

private object GoToRefIdent extends StrictImplicitLogging {

  /**
   * Navigates to the definition of a token in the source code.
   *
   * @param definition The definition to search references for.
   * @param sourceCode The parsed state of the source-code where the search is executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An iterator over the target go-to location(s).
   */
  def goTo(
      definition: Node[Ast.Ident, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToRefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] =
    definition.parent match {
      case Some(parent) =>
        parent match {
          case node @ Node(field: Ast.EnumField[_], _) =>
            // They selected an enum field.
            val result =
              goToEnumFieldUsage(
                node = node.upcast(field),
                sourceCode = sourceCode,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = field.ident,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case node @ Node(field: Ast.EventField, _) =>
            // They selected an event field.
            // Check the parent to find the event references.
            val result =
              goToEventFieldUsages(
                node = node.upcast(field),
                sourceCode = sourceCode,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = field.ident,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case Node(constantDef: Ast.ConstantVarDef[_], _) =>
            val result =
              goToConstantUsage(
                constantDef = constantDef,
                sourceCode = sourceCode,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = constantDef.ident,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case namedVarNode @ Node(namedVar: Ast.NamedVar, _) =>
            // User selected a named variable. Find its usages.
            val result =
              goToLocalVariableUsage(
                fromNode = namedVarNode.upcast(namedVar),
                sourceCode = sourceCode,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = namedVar.ident,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case argumentNode @ Node(argument: Ast.Argument, _) =>
            // They selected an argument. Take 'em there!
            val result =
              goToArgumentUsage(
                argumentNode = argumentNode.upcast(argument),
                sourceCode = sourceCode,
                workspace = workspace,
                includeTemplateArgumentOverrides = settings.includeTemplateArgumentOverrides
              )

            IncludeDeclaration.add(
              definitionAST = argument.ident,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case Node(mapFuncCall: Ast.MapDef, _) =>
            // Go-to MapDef usages.
            val result =
              goToMapDefUsages(
                ident = mapFuncCall.ident,
                sourceCode = sourceCode,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = mapFuncCall.ident,
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
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Ident]] =
    goToNearestBlockInScope(fromNode, sourceCode.tree)
      .iterator
      .flatMap {
        from =>
          goToVariableUsages(
            definition = fromNode.data.ident,
            from = from,
            sourceCode = sourceCode,
            workspace = workspace
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
      workspace: WorkspaceState.IsSourceAware,
      includeTemplateArgumentOverrides: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Ident]] =
    goToNearestFuncDef(argumentNode) match {
      case Some(functionNode) =>
        // It's a function argument, search within this function's body.
        goToVariableUsages(
          definition = argumentNode.data.ident,
          from = functionNode,
          sourceCode = sourceCode,
          workspace = workspace
        )

      case None =>
        // It's a template argument, search within the source-tree and within all dependant code.
        goToTemplateArgumentUsage(
          argument = argumentNode.data,
          sourceCode = sourceCode,
          workspace = workspace,
          includeTemplateArgumentOverrides = includeTemplateArgumentOverrides
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
      workspace: WorkspaceState.IsSourceAware,
      includeTemplateArgumentOverrides: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Ident]] = {
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

    val overriddenArguments =
      if (includeTemplateArgumentOverrides)
        goToOverriddenTemplateArguments(
          argument = argument,
          sourceCode = sourceCode,
          workspace = workspace
        )
      else
        Iterator.empty

    contractInheritanceUsage ++ withInheritanceUsage ++ overriddenArguments
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
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Ident]] =
    WorkspaceSearcher
      .collectImplementingChildren(sourceCode, workspace)
      .childTrees
      .iterator
      .flatMap {
        sourceCode =>
          goToVariableUsages(
            definition = argument.ident,
            from = sourceCode.tree.rootNode,
            sourceCode = sourceCode,
            workspace = workspace
          )
      }

  /**
   * Includes overridden template/Contract level arguments.
   *
   * For example: In the following case the overridden `variable` in `Child` Contract
   * are collect when searching for references on `variable` defined in `Parent` Contract.
   * {{{
   *    Abstract Contract Parent(variable@@: Bool) { }
   *    Abstract Contract Child(>>variable<<: Bool) extends Parent(variable) { }
   * }}}
   *
   * @param argument   The template argument whose overrides are to be searched.
   * @param sourceCode The source where this argument exists.
   * @param workspace  The workspace that may contain other dependant source-code.
   * @return An iterator containing identities representing the locations where the template argument is overridden.
   */
  private def goToOverriddenTemplateArguments(
      argument: Ast.Argument,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Ident]] =
    WorkspaceSearcher
      .collectInheritanceHierarchy(sourceCode, workspace)
      .flatten()
      .iterator
      .flatMap {
        sourceCode =>
          GoToDefIdent.goToTemplateArguments(
            ident = argument.ident,
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
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Ident]] = {
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
            definition = constantDef.ident,
            from = sourceCode.tree.rootNode,
            sourceCode = sourceCode,
            workspace = workspace
          )
      }
  }

  /**
   * Navigate to all variable usages for the given variable identifier.
   *
   * @param definition The variable identifier to search for.
   * @param from  The node to search within, walking downwards.
   * @return An array sequence of variable usage IDs.
   */
  private def goToVariableUsages(
      definition: Ast.Ident,
      from: Node[Ast.Positioned, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Ident]] =
    from
      .walkDown
      .collect {
        // find all the selections matching the variable name.
        case Node(variable: Ast.Variable[_], _) if variable.id == definition =>
          SourceLocation.Node(variable.id, sourceCode)

        // collect all assignments
        case Node(variable: Ast.AssignmentTarget[_], _) if variable.ident == definition =>
          SourceLocation.Node(variable.ident, sourceCode)
      }
      .filter {
        reference =>
          // So far the collected references match the name, but they also must match the actual definition in scope.
          isReferenceForDefinition(
            definition = definition,
            reference = reference,
            sourceCode = sourceCode,
            workspace = workspace
          )
      }

  /**
   * Checks if the input `reference` is in fact a reference for the input `definition`'s instance.
   * There could be duplicate definitions, a go-to-definition test can confirm this.
   *
   * @param definition The definition to expect.
   * @param reference  The reference to test.
   * @param sourceCode The parsed state of the source-code where the search is executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return True if definitions were a match, otherwise false.
   */
  private def isReferenceForDefinition(
      definition: Ast.Ident,
      reference: SourceLocation.Node[Ast.Ident],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Boolean =
    reference.ast.sourceIndex exists {
      referenceSourceIndex =>
        CodeProvider
          .goToDefinition
          .search(
            cursorIndex = referenceSourceIndex.index,
            sourceCode = sourceCode.parsed,
            workspace = workspace,
            searchSettings = ()
          )
          .exists {
            case SourceLocation.File(_) =>
              false

            case SourceLocation.Node(foundDefinition, _) =>
              // The following could be tested with `ast eq ident`
              foundDefinition == definition && foundDefinition.sourceIndex == definition.sourceIndex
          }
    }

  /**
   * Navigate to the nearest function definition for which the given child node is in scope.
   *
   * @param childNode The node to traverse up from.
   * @return An Option containing the nearest function definition, if found.
   */
  private def goToNearestFuncDef(childNode: Node[Ast.Positioned, Ast.Positioned]): Option[Node[Ast.FuncDef[_], Ast.Positioned]] =
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

}
