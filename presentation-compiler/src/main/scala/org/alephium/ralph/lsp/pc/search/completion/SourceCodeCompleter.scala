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

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension

object SourceCodeCompleter {

  /**
   * Navigates to the definition of a token in the source code.
   *
   * @param cursorIndex The position where this request was executed.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @return An iterator over the target go-to location(s).
   */
  def complete(
      cursorIndex: Int,
      sourceCode: SourceLocation.Code): Iterator[Suggestion] =
    sourceCode.tree.rootNode.findLast(_.sourceIndex.exists(_ contains cursorIndex)) match { // find the node closest to this source-index
      case Some(closest) =>
        closest match {
          case functionNode @ Node(function: Ast.FuncDef[_], _) =>
            FunctionBodyCompleter.suggest(
              cursorIndex = cursorIndex,
              functionNode = functionNode.upcast(function),
              sourceCode = sourceCode
            )

          case _ =>
            Iterator.empty
        }

      case None =>
        Iterator.empty
    }

}
