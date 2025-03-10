// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
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
