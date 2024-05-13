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

object FunctionBodyCompleter {

  def suggest(
      cursorIndex: Int,
      functionNode: Node[Ast.FuncDef[_], Ast.Positioned],
      sourceCode: SourceLocation.Code): Iterator[Suggestion] =
    suggestLocalFunctionVariables(
      cursorIndex = cursorIndex,
      functionNode = functionNode,
      sourceCode = sourceCode
    )

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
          val node =
            SourceLocation.Node(
              ast = argument,
              source = sourceCode
            )

          Suggestion.Argument(
            node = node,
            isTemplateArgument = false
          )

        case Node(data: Ast.VarDef[_], _) =>
          Suggestion.VarDef(SourceLocation.Node(data, sourceCode))
      }

}
