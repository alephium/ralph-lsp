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
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeSearcher}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

import scala.collection.immutable.ArraySeq

private[search] object GoToDefIdent {

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
            goToEnumField(
              fieldSelector = fieldSelector,
              sourceCode = sourceCode,
              workspace = workspace
            )

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

          case Node(field: Ast.EnumField[_], _) if field.ident == identNode.data =>
            Iterator(
              SourceLocation.Node(
                ast = field,
                source = sourceCode
              )
            )

          case Node(field: Ast.EventField, _) if field.ident == identNode.data =>
            Iterator(
              SourceLocation.Node(
                ast = field,
                source = sourceCode
              )
            )

          case Node(constantDef: Ast.ConstantVarDef[_], _) if constantDef.ident == identNode.data =>
            Iterator(
              SourceLocation.Node(
                ast = constantDef,
                source = sourceCode
              )
            )

          case Node(namedVar: Ast.NamedVar, _) if namedVar.ident == identNode.data =>
            // User selected a named variable. Find its usages.
            Iterator(
              SourceLocation.Node(
                ast = namedVar,
                source = sourceCode
              )
            )

          case Node(argument: Ast.Argument, _) if argument.ident == identNode.data =>
            // They selected an argument. Take 'em there!
            Iterator(
              SourceLocation.Node(
                ast = argument,
                source = sourceCode
              )
            )

          case Node(mapDef: Ast.MapDef, _) if mapDef.ident == identNode.data =>
            Iterator(
              SourceLocation.Node(
                ast = mapDef,
                source = sourceCode
              )
            )

          case _ =>
            Iterator.empty
        }

      case None =>
        Iterator.empty
    }

  /**
   * Navigate to enum fields.
   *
   * @param fieldSelector The enum field to search.
   * @param sourceCode    The source code location where this request was executed.
   * @param workspace     The workspace where this search was executed and where all the source trees exist.
   * @return An iterator representing the locations of the enum field implementations.
   */
  private def goToEnumField(
      fieldSelector: Ast.EnumFieldSelector[_],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.EnumField[_]]] = {
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

    // collect all trees to search
    val allTrees =
      parents ++ globalEnums

    // find matching enum fields
    allTrees
      .iterator
      .flatMap(goToEnumField(fieldSelector, _))
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
          .flatMap {
            enumDef =>
              findEnumField(
                fieldSelector = fieldSelector,
                enumDef = enumDef,
                sourceCode = sourceCode
              )
          }

      case enumDef: Ast.EnumDef[_] =>
        findEnumField(
          fieldSelector = fieldSelector,
          enumDef = enumDef,
          sourceCode = sourceCode
        ).iterator

      case _: Ast.ContractInterface | _: Ast.TxScript | _: Ast.Struct | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
        Iterator.empty
    }

  /**
   * Find the enum field(s) for the given enum field selector within the given enum definition.
   *
   * @param fieldSelector The selected enum field to find.
   * @param enumDef       The enum definition to search within.
   * @param sourceCode    The source where the enum definition is defined.
   * @return An array sequence of [[Ast.EnumField]]s matching the search result.
   */
  private def findEnumField(
      fieldSelector: Ast.EnumFieldSelector[_],
      enumDef: Ast.EnumDef[_],
      sourceCode: SourceLocation.Code): Option[SourceLocation.Node[Ast.EnumField[_]]] =
    if (enumDef.id == fieldSelector.enumId)
      enumDef
        .fields
        .find(_.ident == fieldSelector.field)
        .map(SourceLocation.Node(_, sourceCode))
    else
      None

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
        GoToDefFuncId.goToNearestFuncDef(childNode).orElse(Some(sourceTree.rootNode))

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
    GoToDefFuncId.goToNearestFuncDef(identNode) match {
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

}
