// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler

import org.alephium.ralph.{CompiledContract, CompiledScript, Warning}
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

sealed trait CompilerRunResult

object CompilerRunResult {

  /**
   * Result of a successful compiler run.
   */
  case class Compiled(
      contracts: Array[CompiledContract],
      scripts: Array[CompiledScript],
      warnings: Array[Warning],
      globalState: CompilerRunGlobalState)
    extends CompilerRunResult

  /**
   * Result of an errored compiler run.
   *
   * @param error       Error representing the exception throw by ralphc.
   * @param globalState Partially successful compilation state.
   */
  case class Errored(
      error: CompilerMessage.AnyError,
      globalState: Option[CompilerRunGlobalState])
    extends CompilerRunResult

}
