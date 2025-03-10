// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
 *  - `alephium.config.ts` i.e. [[TSBuildState.Errored]]
 */
case class BuildError(error: Either[TSBuildState.Errored, BuildState.Errored]) extends AnyVal
