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

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.alephium.ralph.lsp.pc.workspace.build.typescript.TSBuildState

/**
 * Presentation-compiler state.
 *
 * @param workspace   Current workspace state.
 * @param buildErrors Stores current build file errors which are not tied to the current workspace state.
 *                     - On successful build compilation, this gets set to [[None]] and the workspace is updated with the new build file.
 *                     - If there are build errors, the workspace still handles requests using the most recent valid and compiled build file
 *                       until this build file is error-free and successfully compiled.
 * @param tsErrors    Errors to report on TypeScript `alephium.config.ts` build file.
 */
case class PCState(
    workspace: WorkspaceState,
    buildErrors: Option[BuildState.Errored],
    tsErrors: Option[TSBuildState.Errored])
