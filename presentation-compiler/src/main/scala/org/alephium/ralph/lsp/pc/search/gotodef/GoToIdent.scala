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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.ast.{AstExtra, Tree}
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

import scala.collection.immutable.ArraySeq

private[search] object GoToIdent {

  /**
   * Navigate to the argument(s) for the given identifier.
   *
   * @param identNode  The node representing the identifier in the AST.
   * @param sourceCode The source-tree and its parsed source-code state, where this search was executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An array sequence of positioned ASTs matching the search result.
   */
  def goTo(
      identNode: Node[Ast.Ident, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Positioned]] =
    identNode.parent match { // take one step up to check the type of ident node.
      case Some(parent) =>
        parent match {
          case Node(variable: Ast.Variable[_], _) if variable.id == identNode.data =>
            // They selected a variable. Take 'em there!
            goToScopeDefinitions(
              identNode = identNode,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(assignment: Ast.AssignmentTarget[_], _) if assignment.ident == identNode.data =>
            // They selected an assignment. Take 'em there!
            goToScopeDefinitions(
              identNode = identNode,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(fieldSelector: Ast.EnumFieldSelector[_], _) if fieldSelector.field == identNode.data =>
            // They selected an enum field. Take 'em there!
            WorkspaceSearcher
              .collectInheritedParents(sourceCode, workspace)
              .parentTrees
              .iterator
              .flatMap(goToEnumField(fieldSelector, _))

          case Node(mapFuncCall: Ast.MapFuncCall, _) if mapFuncCall.ident == identNode.data =>
            // They selected an map on a function call.
            goToMapFunction(
              ident = identNode.data,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(mapFuncCall: Ast.MapContains, _) if mapFuncCall.ident == identNode.data =>
            // They selected an map on a function call.
            goToMapFunction(
              ident = identNode.data,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(ast: Ast.ContractInheritance, _) if ast.idents.contains(identNode.data) =>
            goToTemplateArgument(
              ident = identNode.data,
              sourceCode = sourceCode
            )

          case node @ Node(field: Ast.EnumField[_], _) if field.ident == identNode.data =>
            // They selected an enum field.
            // Check the parent to find the enum type.
            node
              .parent
              .map(_.data)
              .iterator
              .collect {
                // Check: Parent is an enum definition which contains the enum field.
                case enumDef: Ast.EnumDef[_] if enumDef.fields.exists(_.ident == field.ident) =>
                  WorkspaceSearcher
                    .collectImplementingChildren(sourceCode, workspace)
                    .childTrees
                    .iterator
                    .flatMap {
                      sourceCode =>
                        goToEnumFieldUsage(
                          enumType = enumDef.id,
                          enumField = field,
                          sourceCode = sourceCode
                        )
                    }
              }
              .flatten

          case node @ Node(field: Ast.EventField, _) if field.ident == identNode.data =>
            // They selected an event field.
            // Check the parent to find the event definition.
            node
              .parent
              .map(_.data)
              .iterator
              .collect {
                // Check: Parent is an event definition which contains the event field.
                case eventDef: Ast.EventDef if eventDef.fields.exists(_.ident == field.ident) =>
                  WorkspaceSearcher
                    .collectImplementingChildren(sourceCode, workspace)
                    .childTrees
                    .iterator
                    .flatMap {
                      sourceCode =>
                        goToEventFieldUsage(
                          eventDefId = eventDef.id,
                          eventFieldIndex = eventDef.fields.indexWhere(_.ident == field.ident),
                          sourceCode = sourceCode
                        )
                    }
              }
              .flatten

          case Node(constantDef: Ast.ConstantVarDef[_], _) if constantDef.ident == identNode.data =>
            goToConstantUsage(
              constantDef = constantDef,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case namedVarNode @ Node(namedVar: Ast.NamedVar, _) if namedVar.ident == identNode.data =>
            // User selected a named variable. Find its usages.
            goToLocalVariableUsage(
              fromNode = namedVarNode.upcast(namedVar),
              sourceCode = sourceCode
            )

          case argumentNode @ Node(argument: Ast.Argument, _) if argument.ident == identNode.data =>
            // They selected an argument. Take 'em there!
            goToArgumentUsage(
              argumentNode = argumentNode.upcast(argument),
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(mapFuncCall: Ast.MapDef, _) if mapFuncCall.ident == identNode.data =>
            // Go-to MapDef usages.
            goToMapDefUsages(
              ident = identNode.data,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case _ =>
            Iterator.empty
        }

      case None =>
        Iterator.empty
    }

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
    GoToFuncId.goToNearestFuncDef(argumentNode) match {
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
   * Navigate to arguments, constants and variables for the given identity ([[Ast.Ident]]).
   *
   * @param identNode  The node representing the identity.
   * @param sourceCode The source tree containing the identity node.
   * @param workspace  The workspace in scope, that may contain other dependant source-code.
   * @return An array sequence of arguments, constants and variables with the input identity.
   */
  private def goToScopeDefinitions(
      identNode: Node[Ast.Ident, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Positioned]] = {
    val inheritedParents =
      WorkspaceSearcher.collectInheritedParents(sourceCode, workspace)

    val argumentsAndConstants =
      inheritedParents
        .parentTrees
        .iterator
        .flatMap(goToTemplateDefinitionsAndArguments(identNode.data, _))

    val globalConstants =
      goToGlobalConstants(
        ident = identNode.data,
        trees = inheritedParents.allTrees
      )

    val functionArguments =
      goToNearestFunctionArguments(identNode)
        .map(SourceLocation.Node(_, sourceCode))

    val localVariables =
      goToInScopeVariables(
        identNode = identNode,
        sourceCode = sourceCode
      )

    argumentsAndConstants ++ globalConstants ++ functionArguments ++ localVariables
  }

  /**
   * Navigate to global constants.
   *
   * @param ident The identity of the constant.
   * @param trees The source trees to search within.
   * @return An iterator over the found constants.
   */
  private def goToGlobalConstants(
      ident: Ast.Ident,
      trees: ArraySeq[SourceLocation.Code]): Iterator[SourceLocation.Node[Ast.ConstantVarDef[_]]] =
    trees
      .iterator
      .collect {
        // Global constants are source trees with only one node of type `Ast.ConstantVarDef`
        case source @ SourceLocation.Code(Tree.Source(constant @ Ast.ConstantVarDef(thisIdent, _), _), _) if thisIdent == ident =>
          SourceLocation.Node(
            ast = constant,
            source = source
          )
      }

  /**
   * Navigate to constants and template arguments within the source tree.
   *
   * @param ident      The identity to search for.
   * @param sourceCode The source tree to search within.
   * @return An iterator over the found constants and arguments.
   */
  private def goToTemplateDefinitionsAndArguments(
      ident: Ast.Ident,
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Positioned]] = {
    val constantsAndMaps =
      goToTemplateLevelDefinitions(
        ident = ident,
        sourceCode = sourceCode
      )

    val templateArguments =
      goToTemplateArguments(
        ident = ident,
        sourceCode = sourceCode
      )

    constantsAndMaps ++ templateArguments
  }

  /**
   * Navigate to the constant definitions for the given identifier.
   *
   * @param ident      The constant identification to find.
   * @param sourceCode The source tree to search within.
   * @return The constant definitions.
   */
  private def goToTemplateLevelDefinitions(
      ident: Ast.Ident,
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Positioned]] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .map(_.data)
      .collect {
        case constant: Ast.ConstantVarDef[_] if constant.ident == ident =>
          SourceLocation.Node(constant, sourceCode)

        case mapDef: Ast.MapDef if mapDef.ident == ident =>
          SourceLocation.Node(mapDef, sourceCode)
      }

  /**
   * Navigate to the variable definitions within its scope.
   *
   * @param identNode  The node within a function where the search starts.
   * @param sourceCode The source tree to search within.
   * @return Variable definitions containing the named variable.
   */
  private def goToInScopeVariables(
      identNode: Node[Ast.Ident, Ast.Positioned],
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.VarDef[_]]] =
    goToNearestBlockInScope(identNode, sourceCode.tree)
      .iterator
      .flatMap {
        block =>
          block
            .walkDown
            .collect {
              case Node(varDef: Ast.VarDef[_], _) if AstExtra.containsNamedVar(varDef, identNode.data) =>
                SourceLocation.Node(varDef, sourceCode)
            }
      }

  /**
   * Navigate to the enum field(s) for the given selected enum field.
   *
   * @param fieldSelector The selected enum field to find.
   * @param sourceCode    The source tree to search within.
   * @return An array sequence of [[Ast.EnumField]]s matching the search result.
   */
  private def goToEnumField(
      fieldSelector: Ast.EnumFieldSelector[_],
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.EnumField[_]]] =
    sourceCode.tree.ast match {
      case contract: Ast.Contract =>
        contract
          .enums
          .iterator
          .filter(_.id == fieldSelector.enumId)
          .flatMap(_.fields.find(_.ident == fieldSelector.field))
          .map(SourceLocation.Node(_, sourceCode))

      case _: Ast.ContractInterface | _: Ast.TxScript | _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
        Iterator.empty
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
        GoToFuncId.goToNearestFuncDef(childNode).orElse(Some(sourceTree.rootNode))

      case _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
        None
    }

  /**
   * Navigate to the argument(s) of the nearest function to this node.
   *
   * @param identNode The node to traverse up in search of the function.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   */
  private def goToNearestFunctionArguments(identNode: Node[Ast.Ident, Ast.Positioned]): Iterator[Ast.Argument] =
    GoToFuncId.goToNearestFuncDef(identNode) match {
      case Some(functionNode) =>
        functionNode
          .data
          .args
          .iterator
          .filter(_.ident == identNode.data)

      case None =>
        Iterator.empty
    }

  /**
   * Navigate to the template argument(s) for the given identifier.
   *
   * @param ident      The variable identifier to find arguments for.
   * @param sourceCode The source tree to search within.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   */
  private def goToTemplateArguments(
      ident: Ast.Ident,
      sourceCode: SourceLocation.Code): Seq[SourceLocation.Node[Ast.Argument]] = {
    val arguments =
      sourceCode.tree.ast match {
        case ast: Ast.TxScript =>
          ast.templateVars

        case contract: Ast.Contract =>
          contract.templateVars ++ contract.fields

        case _: Ast.ContractInterface | _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
          Seq.empty
      }

    arguments
      .filter(_.ident == ident)
      .map(SourceLocation.Node(_, sourceCode))
  }

  /**
   * Navigates to the map definition(s) for the given identifier.
   *
   * @param ident      The identifier of the variable to find the map definition for.
   * @param sourceCode The source tree where this search is executed.
   * @param workspace  The workspace where this search is executed and where all source trees exist.
   * @return An iterator over [[Ast.MapDef]] objects matching the search results.
   */
  private def goToMapFunction(
      ident: Ast.Ident,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.MapDef]] =
    WorkspaceSearcher
      .collectInheritedParents(sourceCode, workspace)
      .parentTrees
      .iterator
      .flatMap {
        code =>
          code.tree.rootNode.walkDown.collect {
            case Node(mapDef: Ast.MapDef, _) if mapDef.ident == ident =>
              SourceLocation.Node(mapDef, code)
          }
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
              SourceLocation.Node(mapCall, code)

            case Node(mapCall: Ast.MapContains, _) if mapCall.ident == ident =>
              SourceLocation.Node(mapCall, code)

            case Node(selector @ Ast.LoadDataBySelectors(Ast.Variable(variableIdent), _), _) if variableIdent == ident =>
              SourceLocation.Node(selector, code)

            case Node(mapCall: Ast.AssignmentSelectedTarget[_], _) if mapCall.ident == ident =>
              SourceLocation.Node(mapCall, code)
          }
      }

  /**
   * Navigate to the template argument definition(s) for the given identifier.
   *
   * @param ident      The identifier of the argument name to find.
   * @param sourceCode The source tree where this search is executed.
   * @return An iterator over template arguments representing the locations where a
   *         template argument with the given identifier is defined.
   */
  private def goToTemplateArgument(
      ident: Ast.Ident,
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Argument]] =
    sourceCode.tree.rootNode.walkDown.collect {
      // ident should match and that argument's parent must be a top level object i.e. a Contract, Abstract Contract etc
      case node @ Node(ast: Ast.Argument, _) if ast.ident == ident && node.parent.exists(_.data == sourceCode.tree.rootNode.data) =>
        SourceLocation.Node(ast, sourceCode)
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

}
