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

import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

object TypeCompleter {

  /**
   * Suggests all types available in the provided workspace.
   *
   * @param workspace The workspace to search for types.
   * @return An iterator of all types in the workspace.
   */
  def suggest(workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.Type] = {
    val workspaceTypes =
      WorkspaceSearcher
        .collectTypes(workspace = workspace, includeNonImportedCode = true)
        .map(Suggestion.CreatedType)

    val primitiveTypes =
      PrimitiveTypesCompleter.suggest()

    workspaceTypes ++ primitiveTypes
  }

}
