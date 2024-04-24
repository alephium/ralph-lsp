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
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

private object GoToIdent {

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

          case Node(assignment: Ast.AssignmentSimpleTarget[_], _) if assignment.ident == identNode.data =>
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
              .iterator
              .flatMap(goToEnumField(fieldSelector, _))

          case node @ Node(field: Ast.EnumField, _) if field.ident == identNode.data =>
            // They selected an enum field.
            // Check the parent to find the enum type.
            node
              .parent
              .map(_.data)
              .iterator
              .collect {
                // Check: Parent is an enum definition which contains the enum field.
                case enumDef: Ast.EnumDef if enumDef.fields.exists(_.ident == field.ident) =>
                  WorkspaceSearcher
                    .collectImplementingChildren(sourceCode, workspace)
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

          case Node(constantDef: Ast.ConstantVarDef, _) if constantDef.ident == identNode.data =>
            // They selected a constant definition. Take 'em there!
            WorkspaceSearcher
              .collectImplementingChildren(sourceCode, workspace)
              .iterator
              .flatMap {
                sourceCode =>
                  goToVariableUsages(
                    ident = constantDef.ident,
                    from = sourceCode.tree.rootNode
                  ).map(SourceLocation.Node(_, sourceCode))
              }

          case namedVarNode @ Node(namedVar: Ast.NamedVar, _) if namedVar.ident == identNode.data =>
            // User selected a named variable. Find its usages.
            goToLocalVariableUsage(
              fromNode = namedVarNode.upcast(namedVar),
              sourceCode = sourceCode
            ).map(SourceLocation.Node(_, sourceCode))

          case argumentNode @ Node(argument: Ast.Argument, _) if argument.ident == identNode.data =>
            // They selected an argument. Take 'em there!
            goToArgumentUsage(
              argumentNode = argumentNode.upcast(argument),
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
      sourceCode: SourceLocation.Code): Iterator[Ast.Ident] =
    goToNearestBlockInScope(fromNode, sourceCode.tree)
      .iterator
      .flatMap {
        from =>
          goToVariableUsages(
            ident = fromNode.data.ident,
            from = from
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
          from = functionNode
        ).map(SourceLocation.Node(_, sourceCode))

      case None =>
        // It's a template argument, search within the source-tree and within all dependant code.
        WorkspaceSearcher
          .collectImplementingChildren(sourceCode, workspace)
          .iterator
          .flatMap {
            sourceCode =>
              goToVariableUsages(
                ident = argumentNode.data.ident,
                from = sourceCode.tree.rootNode
              ).map(SourceLocation.Node(_, sourceCode))
          }
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
    val argumentsAndConstants =
      WorkspaceSearcher
        .collectInheritedParents(sourceCode, workspace)
        .iterator
        .flatMap(goToConstantsAndTemplateArguments(identNode.data, _))

    val functionArguments =
      goToNearestFunctionArguments(identNode)
        .map(SourceLocation.Node(_, sourceCode))

    val localVariables =
      goToInScopeVariables(
        identNode = identNode,
        sourceCode = sourceCode
      )

    argumentsAndConstants ++ functionArguments ++ localVariables
  }

  /**
   * Navigate to constants and template arguments within the source tree.
   *
   * @param ident      The identity to search for.
   * @param sourceCode The source tree to search within.
   * @return An iterator over the found constants and arguments.
   */
  private def goToConstantsAndTemplateArguments(
      ident: Ast.Ident,
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Positioned]] = {
    val constants =
      goToConstants(
        ident = ident,
        sourceCode = sourceCode
      )

    val templateArguments =
      goToTemplateArguments(
        ident = ident,
        sourceCode = sourceCode
      )

    constants ++ templateArguments
  }

  /**
   * Navigate to the constant definitions for the given identifier.
   *
   * @param ident      The constant identification to find.
   * @param sourceCode The source tree to search within.
   * @return The constant definitions.
   */
  private def goToConstants(
      ident: Ast.Ident,
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.ConstantVarDef]] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .map(_.data)
      .collect {
        case constant: Ast.ConstantVarDef if constant.ident == ident =>
          SourceLocation.Node(constant, sourceCode)
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
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.EnumField]] =
    sourceCode.tree.ast match {
      case Left(contract: Ast.Contract) =>
        contract
          .enums
          .iterator
          .filter(_.id == fieldSelector.enumId)
          .flatMap(_.fields.find(_.ident == fieldSelector.field))
          .map(SourceLocation.Node(_, sourceCode))

      case Left(_: Ast.ContractInterface | _: Ast.TxScript) | Right(_: Ast.Struct) =>
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
      enumField: Ast.EnumField,
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
      from: Node[Ast.Positioned, Ast.Positioned]): Iterator[Ast.Ident] =
    from
      .walkDown
      .collect {
        // find all the selections matching the variable name.
        case Node(variable: Ast.Variable[_], _) if variable.id == ident =>
          variable.id
      }

  /**
   * Navigate to the nearest code block for which the given child node is in scope.
   *
   * @param childNode The node within the scoped block.
   * @return An Option containing the nearest parent block, if found.
   */
  private def goToNearestBlockInScope(
      childNode: Node[Ast.Positioned, Ast.Positioned],
      sourceTree: Tree.Source): Option[Node[Ast.Positioned, Ast.Positioned]] =
    sourceTree.ast match {
      case Left(_: Ast.Contract | _: Ast.ContractInterface | _: Ast.TxScript) =>
        // Find the nearest function definition or use the template body as the scope.
        goToNearestFuncDef(childNode).orElse(Some(sourceTree.rootNode))

      case Right(_: Ast.Struct) =>
        None
    }

  /**
   * Navigate to the nearest function definition for which the given child node is in scope.
   *
   * @param childNode The node to traverse up from.
   * @return An Option containing the nearest function definition, if found.
   */
  private def goToNearestFuncDef(childNode: Node[Ast.Positioned, Ast.Positioned]): Option[Node[Ast.FuncDef[_], Ast.Positioned]] =
    childNode
      .data
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

  /**
   * Navigate to the argument(s) of the nearest function to this node.
   *
   * @param identNode The node to traverse up in search of the function.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   */
  private def goToNearestFunctionArguments(identNode: Node[Ast.Ident, Ast.Positioned]): Iterator[Ast.Argument] =
    goToNearestFuncDef(identNode) match {
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
        case Left(contract) =>
          contract match {
            case ast: Ast.TxScript =>
              ast.templateVars

            case contract: Ast.Contract =>
              contract.templateVars ++ contract.fields

            case _: Ast.ContractInterface =>
              Seq.empty
          }

        case Right(_) =>
          Seq.empty
      }

    arguments
      .filter(_.ident == ident)
      .map(SourceLocation.Node(_, sourceCode))
  }

}
