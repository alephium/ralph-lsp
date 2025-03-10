// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename

import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

/**
 * Implements [[CodeProvider]] that provides renaming results of type [[SourceLocation.GoToRenameStrict]].
 */
private[search] case object GoToRenameProvider extends CodeProvider[SourceCodeState.Parsed, Unit, SourceLocation.GoToRenameStrict] {

  /** @inheritdoc */
  override def search(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: Unit
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToRenameStrict] =
    GoToRenameAll.rename(
      cursorIndex = cursorIndex,
      sourceCode = sourceCode,
      workspace = workspace
    )

}
