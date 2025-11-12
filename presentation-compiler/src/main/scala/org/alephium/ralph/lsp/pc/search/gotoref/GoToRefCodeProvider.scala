// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

/**
 * Implements [[CodeProvider]] that provides go-to references results of type [[SourceLocation.GoToRefStrict]].
 */
private[search] case object GoToRefCodeProvider
  extends CodeProvider[SourceCodeState.IsParsed, (SoftAST.type, GoToRefSetting), SourceLocation.GoToRefSoft]
     with StrictImplicitLogging {

  /** @inheritdoc */
  override def searchLocal(
      cursorIndex: Int,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: (SoftAST.type, GoToRefSetting)
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.GoToRefSoft] =
    // find the statement where this cursorIndex sits.
    GoToRefIdentifier
      .searchLocal(
        cursorIndex = cursorIndex,
        sourceCode = sourceCode,
        workspace = workspace,
        searchSettings = searchSettings._2
      )
      .iterator

}
