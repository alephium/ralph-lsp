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

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

object FunctionBodyCompleter {

  /**
   * Provides suggestions available at the given position within the body of a function.
   *
   * @param cursorIndex  The position where this search was executed.
   * @param functionNode The node representing the function where this search was executed.
   * @param sourceCode   The source code containing the given function.
   * @param workspace    The workspace containing the source code.
   * @return An iterator over suggestions available before the cursor position within the function.
   */
  def suggest(
      cursorIndex: Int,
      functionNode: Node[Ast.FuncDef[_], Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.NodeAPI] = {
    // fetch suggestions local to this function
    val localFunctionSuggestions =
      suggestLocalFunctionVariables(
        cursorIndex = cursorIndex,
        functionNode = functionNode,
        sourceCode = sourceCode
      )

    // fetch suggestions available within this function due to inheritance.
    val inheritedSuggestions =
      suggestInheritedAPIs(
        sourceCode = sourceCode,
        workspace = workspace
      )

    val builtInFunctions =
      suggestBuiltinFunctions(workspace)

    localFunctionSuggestions ++
      inheritedSuggestions ++
      builtInFunctions
  }

  /**
   * Suggests variables local to the given function.
   *
   * @param cursorIndex  The position where this request was executed.
   * @param functionNode The node representing the function where this request was executed.
   * @param sourceCode   The source code to which this function belongs.
   * @return An iterator of suggestions for function arguments and local variables
   *         within the function.
   */
  private def suggestLocalFunctionVariables(
      cursorIndex: Int,
      functionNode: Node[Ast.FuncDef[_], Ast.Positioned],
      sourceCode: SourceLocation.Code): Iterator[Suggestion.NodeAPI] =
    functionNode
      .walkDown
      .filter(_.data.sourceIndex.exists(_.from <= cursorIndex))
      .collect {
        case Node(argument: Ast.Argument, _) =>
          Suggestion.Argument(
            node = SourceLocation.Node(ast = argument, source = sourceCode),
            isTemplateArgument = false
          )

        case Node(data: Ast.VarDef[_], _) =>
          Suggestion.VarDef(SourceLocation.Node(data, sourceCode))
      }

  /**
   * Suggests public APIs available from inherited code.
   *
   * @param sourceCode The code where this function exists.
   * @param workspace  The workspace state where the source code is located.
   * @return An iterator over suggestions from inherited APIs.
   */
  private def suggestInheritedAPIs(
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.InheritedAPI] =
    WorkspaceSearcher
      .collectInheritedParents(
        sourceCode = sourceCode,
        workspace = workspace
      )
      .iterator
      .flatMap(suggestInheritedAPIs)

  /**
   * Suggests all public APIs from this code available to implementing contracts.
   *
   * @param sourceCode The source code that can be inherited.
   * @return An iterator over public APIs available to inherited code.
   */
  private def suggestInheritedAPIs(sourceCode: SourceLocation.Code): Iterator[Suggestion.InheritedAPI] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .collect {
        case node @ Node(argument: Ast.Argument, _) if node.parent.exists(_.data.isInstanceOf[Ast.ContractWithState]) =>
          // suggest template level arguments
          Suggestion.Argument(
            node = SourceLocation.Node(argument, sourceCode),
            isTemplateArgument = true
          )

        case Node(function: Ast.FuncDef[_], _) =>
          // suggest function names
          Suggestion.FuncDef(
            node = SourceLocation.Node(function, sourceCode),
            isBuiltIn = false
          )

        case Node(eventDef: Ast.EventDef, _) =>
          // suggest events
          Suggestion.EventDef(SourceLocation.Node(eventDef, sourceCode))

        case Node(enumDef: Ast.EnumDef[_], _) =>
          // suggest enums
          Suggestion.EnumDef(SourceLocation.Node(enumDef, sourceCode))

        case Node(constantVarDef: Ast.ConstantVarDef[_], _) =>
          // suggest constants
          Suggestion.ConstantVarDef(SourceLocation.Node(constantVarDef, sourceCode))

        case Node(mapDef: Ast.MapDef, _) =>
          // suggest maps
          Suggestion.MapDef(SourceLocation.Node(mapDef, sourceCode))
      }

  /**
   * Suggests built-in functions available to the workspace as a dependency.
   *
   * @param workspace The workspace that contains the built-in dependency.
   * @return Iterator over built-in functions.
   */
  private def suggestBuiltinFunctions(workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.FuncDef] =
    workspace.build.findDependency(DependencyID.BuiltIn) match {
      case Some(builtIn) =>
        WorkspaceSearcher
          .collectTrees(builtIn)
          .iterator
          .flatMap {
            source =>
              suggestFunctions(
                source = source,
                isBuiltInSource = true
              )
          }

      case None =>
        Iterator.empty
    }

  /**
   * Suggests all functions available in the given source code.
   *
   * @param source          The source code that may contain function definitions.
   * @param isBuiltInSource True if the given source is from the built-in dependency, false otherwise.
   * @return An iterator over the suggested functions.
   */
  private def suggestFunctions(
      source: SourceLocation.Code,
      isBuiltInSource: Boolean): Iterator[Suggestion.FuncDef] =
    source.tree.ast match {
      case Left(contract) =>
        contract
          .funcs
          .iterator
          .map {
            function =>
              val node =
                SourceLocation.Node(
                  ast = function,
                  source = source
                )

              Suggestion.FuncDef(
                node = node,
                isBuiltIn = isBuiltInSource
              )
          }

      case Right(_) =>
        Iterator.empty
    }

}
