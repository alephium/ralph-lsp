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

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.utils.log.ClientLogger

object SourceCodeCompleter {

  /**
   * Provides code completion suggestions at the given cursor index within the source code.
   *
   * @param cursorIndex The index representing the cursor position in the source code.
   * @param sourceCode  The source code where the completion is requested.
   * @param workspace   The workspace state containing the source code.
   * @return An iterator over code completion suggestions.
   */
  def complete(
      cursorIndex: Int,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[Suggestion] =
    sourceCode.tree.rootNode.findLast(_.sourceIndex.exists(_ contains cursorIndex)) match { // find the node closest to this source-index
      case Some(node @ Node(ident: Ast.Ident, _)) =>
        IdentCompleter.suggest(
          cursorIndex = cursorIndex,
          ident = node.upcast(ident),
          sourceCode = sourceCode,
          workspace = workspace
        )

      case Some(node @ Node(funcId: Ast.FuncId, _)) =>
        FuncIdCompleter.suggest(
          cursorIndex = cursorIndex,
          funcId = node.upcast(funcId),
          sourceCode = sourceCode,
          workspace = workspace
        )

      case Some(Node(_: Ast.TypeId | _: Ast.Argument | _: Ast.MapDef, _)) =>
        TypeCompleter.suggest(workspace)

      case Some(node @ Node(_: Ast.Const[_], _)) if node.parent.exists(_.data.isInstanceOf[Ast.AnnotationField[_]]) =>
        // Request is for an annotation value. Eg: `@using(updateFields = tr@@ue)`
        AnnotationCompleter.suggestAnnotationValues()

      case Some(Node(_: Ast.ContractWithState, _)) =>
        // FIXME: At the moment there is no AST that represents the code immediately after the contract definition and before the contract body.
        //        Therefore, all keywords all `pub`, `fn`, `extends`, `implements` etc are suggested in both cases.
        //        See PR: https://github.com/alephium/ralph-lsp/pull/221.
        ContractBodyCompleter.suggest()

      case Some(closest) =>
        FunctionBodyCompleter.suggest(
          cursorIndex = cursorIndex,
          closestToCursor = closest,
          sourceCode = sourceCode,
          workspace = workspace
        )

      case None =>
        Iterator.empty
    }

}
