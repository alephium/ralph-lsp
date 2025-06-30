// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler

import org.alephium.ralph.{CompiledContract, CompiledScript, Warning}

/**
 * Result of a completed compiler run.
 */
case class CompilerRunResult(
    contracts: Array[CompiledContract],
    scripts: Array[CompiledScript],
    warnings: Array[Warning],
    globalState: CompilerRunGlobalState)
