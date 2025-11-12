// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename

import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

/**
 * Implements [[CodeProvider]] that provides renaming results of type [[SourceLocation.GoToRenameSoft]].
 */
private[search] case object GoToRenameCodeProvider extends CodeProvider[SourceCodeState.Parsed, Unit, SourceLocation.GoToRenameSoft] {

  /** @inheritdoc */
  override def searchLocal(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: Unit
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.GoToRenameSoft] =
    GoToRenameAll.rename(
      cursorIndex = cursorIndex,
      sourceCode = sourceCode,
      workspace = workspace
    )

}
