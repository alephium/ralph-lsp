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
import org.alephium.ralph.lsp.utils.log.ClientLogger

object FuncIdCompleter {

  def suggest(
      cursorIndex: Int,
      funcId: Node[Ast.FuncId, Ast.Positioned],
      sourceCode: SourceLocation.Code,
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
