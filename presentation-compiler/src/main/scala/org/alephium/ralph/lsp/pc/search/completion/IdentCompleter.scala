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

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.pc.log.ClientLogger

object IdentCompleter {

  /**
   * Provides code completion suggestions for the given identity being the closest node
   * to the cursor index.
   *
   * @param cursorIndex The position where this search was executed.
   * @param ident       The node closest to the cursor index.
   * @param sourceCode  The source code where the completion is requested.
   * @param workspace   The workspace containing the source code.
   * @return An iterator over code completion suggestions.
   */
  def suggest(
      cursorIndex: Int,
      ident: Node[Ast.Ident, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[Suggestion] =
    ident.parent match {
      case Some(Node(selector: Ast.EnumFieldSelector[_], _)) =>
        EnumFieldCompleter.suggest(
          enumId = selector.enumId,
          sourceCode = sourceCode,
          workspace = workspace
        )

      case Some(parent @ Node(_: Ast.IdentSelector, _)) =>
        parent.parent match {
          case Some(Node(selector: Ast.LoadDataBySelectors[_], _)) =>
            ExprCompleter.suggest(
              expr = selector.base,
              workspace = workspace
            )

          case _ =>
            Iterator.empty
        }

      case Some(Node(_: Ast.Annotation[_], _)) =>
        AnnotationCompleter.suggestAnnotationNames()

      case Some(Node(_: Ast.AnnotationField[_], _)) =>
        AnnotationCompleter.suggestAnnotationKeys()

      case _ =>
        FunctionBodyCompleter.suggest(
          cursorIndex = cursorIndex,
          closestToCursor = ident,
          sourceCode = sourceCode,
          workspace = workspace
        )
    }

}
