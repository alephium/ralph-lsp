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
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

object ExprCompleter extends StrictImplicitLogging {

  def suggest(
      expr: Ast.Expr[_],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[Suggestion.FuncDef] =
    expr.getCachedType() match {
      case Some(types) =>
        WorkspaceSearcher
          .collectFunctions(
            types = types,
            workspace = workspace
          )
          .map(Suggestion.FuncDef(_, isBuiltIn = false))

      case None =>
        // TODO: The name/identity of the type being suggested should also be reports in this log.
        //       But `Expr` does not have a function that provides such data yet.
        logger.info(s"Code completion unsuccessful: Type inference unresolved. Check for syntax or compilation errors.")
        Iterator.empty
    }

}
