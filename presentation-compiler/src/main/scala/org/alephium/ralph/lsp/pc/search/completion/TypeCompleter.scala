// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

object TypeCompleter {

  /**
   * Suggests all types available in the provided workspace.
   *
   * @param workspace The workspace to search for types.
   * @return An iterator of all types in the workspace.
   */
  def suggest(workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.Type] =
    WorkspaceSearcher
      .collectTypes(workspace = workspace, includeNonImportedCode = true)
      .map(Suggestion.Type)

}
