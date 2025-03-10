// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.Node
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
      sourceCode: SourceLocation.CodeStrict,
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
