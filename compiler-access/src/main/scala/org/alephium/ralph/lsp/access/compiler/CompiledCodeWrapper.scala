// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler

import org.alephium.ralph.{CompiledScript, Warning, CompiledContract, Ast}
import org.alephium.util.AVector

object CompiledCodeWrapper {

  def apply(contract: CompiledContract): CompiledCodeWrapper =
    CompiledCodeWrapper(Left(contract))

  def apply(contract: CompiledScript): CompiledCodeWrapper =
    CompiledCodeWrapper(Right(contract))

}

/**
 * A wrapper class for compilation result types returned by ralphc.
 *
 * This is needed because the types [[CompiledContract]] and [[CompiledScript]] do not have a subtype.
 *
 * TODO: Remove this class when a subtype is available.
 *
 * @param either Either a contract or a script.
 */
case class CompiledCodeWrapper(either: Either[CompiledContract, CompiledScript]) extends AnyVal {

  def warnings: AVector[Warning] =
    either match {
      case Left(contract) =>
        contract.warnings

      case Right(script) =>
        script.warnings
    }

  def ast: Ast.ContractWithState =
    either match {
      case Left(contract) =>
        contract.ast

      case Right(script) =>
        script.ast
    }

}
