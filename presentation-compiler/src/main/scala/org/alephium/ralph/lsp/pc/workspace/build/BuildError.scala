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

package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.pc.workspace.build.typescript.TSBuildState

object BuildError {

  @inline def apply(tsError: TSBuildState.Errored): BuildError =
    BuildError(Left(tsError))

  @inline def apply(jsonError: BuildState.Errored): BuildError =
    BuildError(Right(jsonError))

}

/**
 * A build error wrapper that can either be within the following files:
 *  - `ralph.json` i.e. [[BuildState.Errored]]
 *  - `alephium.config.json` i.e. [[TSBuildState.Errored]]
 */
case class BuildError(error: Either[TSBuildState.Errored, BuildState.Errored]) extends AnyVal
