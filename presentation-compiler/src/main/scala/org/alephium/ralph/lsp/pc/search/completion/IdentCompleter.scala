// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.ClientLogger

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
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[Suggestion] =
    ident.parent match {
      case Some(Node(selector: Ast.EnumFieldSelector[_], _)) =>
        EnumFieldCompleter.suggest(
          enumId = selector.enumId,
          sourceCode = sourceCode,
          workspace = workspace
        )

      case Some(parent @ Node(_: Ast.IdentSelector[_], _)) =>
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
