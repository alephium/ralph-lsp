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
import org.alephium.protocol.vm.StatefulContext
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
      parsedSource: Seq[Ast.MultiContractDef],
      options: CompilerOptions,
      workspaceErrorURI: URI): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])] =
    try {
      val allContracts     = Seq.newBuilder[Ast.ContractWithState]
      val otherDefinitions = Seq.newBuilder[Ast.GlobalDefinition]

      parsedSource foreach {
        case contract: Ast.ContractWithState =>
          contract.reset() // Reset contracts state before compilation. This is necessary to ensure that contracts are fully recompiled.
          allContracts addOne contract

        case struct: Ast.Struct =>
          otherDefinitions addOne struct

        case varDef: Ast.ConstantVarDef[_] =>
          otherDefinitions addOne varDef.asInstanceOf[Ast.ConstantVarDef[StatefulContext]]

        case enumDef: Ast.EnumDef[_] =>
          otherDefinitions addOne enumDef.asInstanceOf[Ast.EnumDef[StatefulContext]]
      }

      val globalState =
        Ast.GlobalState.from[StatefulContext](otherDefinitions.result())

      val multiContract =
        Ast.MultiContract(
          contracts = allContracts.result(),
          globalState = globalState,
          dependencies = None,
          methodSelectorTable = None
        )

      val extendedContracts =
        multiContract.extendedContracts()

      // TODO: Handle workspace level warning returned here
      val statefulContracts =
        extendedContracts
          .genStatefulContracts()(options)
          ._2
          .map(_._1)

      val statefulScripts =
        extendedContracts.genStatefulScripts()(options)

      Right((statefulContracts.toArray, statefulScripts.toArray))
    } catch TryUtil.catchAllThrows(workspaceErrorURI)

}
