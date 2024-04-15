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

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.{CompiledScript, Ast, CompilerOptions, CompiledContract}

import java.net.URI

object CompilerAccess {

  val RALPH_FILE_EXTENSION = "ral"

  def ralphc: CompilerAccess =
    RalphCompilerAccess

}

/**
 * Defines functions that perform compiler specific IO operation.
 *
 * @note These functions are mostly in-memory operation and do not
 *       perform file-io.
 * @see [[org.alephium.ralph.lsp.access.file.FileAccess]] for file-io.
 */
trait CompilerAccess {

  /**
   * Runs the parser phase.
   *
   * @param code Code to parse.
   * @return Parsing error or successfully parsed AST.
   */
  def parseContracts(
      fileURI: URI,
      code: String): Either[CompilerMessage.AnyError, Tree.Root]

  /**
   * Given the parsed AST and compiler options, compile the contracts.
   *
   * @param workspaceErrorURI The [[URI]] to report errors to when `fileURI` is absent from compilation errors.
   */
  def compileContracts(
      contracts: Seq[Ast.ContractWithState],
      structs: Seq[Ast.Struct],
      options: CompilerOptions,
      workspaceErrorURI: URI): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])]

}
