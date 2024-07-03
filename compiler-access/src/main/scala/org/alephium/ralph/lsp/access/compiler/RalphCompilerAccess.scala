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

import fastparse.Parsed
import org.alephium.ralph._
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error._
import org.alephium.ralph.lsp.access.util.TryUtil

import java.net.URI

/**
 * Implements ralph parsing and compilation functions accessing the `ralphc`.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object RalphCompilerAccess extends CompilerAccess {

  /** @inheritdoc */
  def parseContracts(
      fileURI: URI,
      code: String): Either[CompilerMessage.AnyError, Tree.Root] =
    try
      fastparse.parse(code, RalphParserExtension.multiContract(fileURI)(_)) match {
        case Parsed.Success(source: Tree.Root, _) =>
          Right(source)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }
    catch TryUtil.catchAllThrows(fileURI)

  /** @inheritdoc */
  def compileContracts(
      contracts: Seq[Ast.ContractWithState],
      structs: Seq[Ast.Struct],
      options: CompilerOptions,
      workspaceErrorURI: URI): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])] =
    try {
      val fixedContracts = InconsistentWarningsFix.fix(contracts, structs, options)

      val multiContract =
        Ast.MultiContract(fixedContracts, structs, None, None)

      val extendedContracts =
        multiContract.extendedContracts()

      val statefulContracts =
        extendedContracts
          .genStatefulContracts()(options)
          .map(_._1)

      val statefulScripts =
        extendedContracts.genStatefulScripts()(options)

      Right((statefulContracts.toArray, statefulScripts.toArray))
    } catch TryUtil.catchAllThrows(workspaceErrorURI)

}
