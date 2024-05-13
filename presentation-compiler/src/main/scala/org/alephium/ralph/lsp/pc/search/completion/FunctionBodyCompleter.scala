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
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

object FunctionBodyCompleter {

  def suggest(
      cursorIndex: Int,
      functionNode: Node[Ast.FuncDef[_], Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion] = {
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

    localFunctionSuggestions ++
      inheritedSuggestions
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
      sourceCode: SourceLocation.Code): Iterator[Suggestion] =
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
      workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion] =
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
          Suggestion.Function(SourceLocation.Node(function, sourceCode))

        case Node(eventDef: Ast.EventDef, _) =>
          // suggest events
          Suggestion.EventDef(SourceLocation.Node(eventDef, sourceCode))

        case Node(enumDef: Ast.EnumDef[_], _) =>
          // suggest enums
          Suggestion.EnumDef(SourceLocation.Node(enumDef, sourceCode))

        case Node(constantVarDef: Ast.ConstantVarDef[_], _) =>
          // suggest constants
          Suggestion.ConstantVarDef(SourceLocation.Node(constantVarDef, sourceCode))
      }

}