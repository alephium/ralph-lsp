// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.ClientLogger

object FuncIdCompleter {

  def suggest(
      cursorIndex: Int,
      funcId: Node[Ast.FuncId, Ast.Positioned],
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[Suggestion] =
    funcId.parent match {
      case Some(Node(call: Ast.ContractCallBase, _)) =>
        ExprCompleter.suggest(
          expr = call.obj,
          workspace = workspace
        )

      case _ =>
        FunctionBodyCompleter.suggest(
          cursorIndex = cursorIndex,
          closestToCursor = funcId,
          sourceCode = sourceCode,
          workspace = workspace
        )
    }

}
