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
