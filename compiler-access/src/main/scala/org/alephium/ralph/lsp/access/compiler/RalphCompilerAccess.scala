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
import org.alephium.ralph.lsp.access.compiler.message.error.FastParseError
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.SoftParser
import org.alephium.ralph.lsp.access.util.TryUtil
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import java.net.URI

/**
 * Implements ralph parsing and compilation functions accessing the `ralphc`.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly access this code.
 */

private object RalphCompilerAccess extends CompilerAccess with StrictImplicitLogging {

  /** @inheritdoc */
  override def parseSoft(code: String): Either[FastParseError, SoftAST.RootBlock] =
    fastparse.parse(code, SoftParser.parse(_)) match {
      case Parsed.Success(ast: SoftAST.RootBlock, _) =>
        Right(ast)

      case failure: Parsed.Failure =>
        Left(FastParseError(failure))
    }

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
      parsedSource: Seq[Ast.GlobalDefinition],
      options: CompilerOptions,
      workspaceErrorURI: URI
    )(implicit logger: ClientLogger): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript], Array[Warning])] =
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

        case _: Ast.AssetScript =>
        // Ignored until the multi-contract parser supports it
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

      val (warnings, statefulContractsAndIndex) =
        extendedContracts.genStatefulContracts()(options)

      // Ensure warnings with empty file information are logged so they can be resolved in Node.
      warnings foreach {
        warning =>
          if (warning.sourceIndex.flatMap(_.fileURI).isEmpty)
            logger.error(CompilerLogMessage.UnassignedWarning(warning))
      }

      val statefulContracts =
        statefulContractsAndIndex.map(_._1).toArray

      val statefulScripts =
        extendedContracts.genStatefulScripts()(options).toArray

      val extractedContractWarnings =
        extractWarnings(statefulContracts.map(CompiledCodeWrapper(_)))

      val extractedScriptWarnings =
        extractWarnings(statefulScripts.map(CompiledCodeWrapper(_)))

      // Combine all warnings into a single collection so there is only one way to handle Warnings by presentation-compiler.
      // This list contains all Warnings with SourceIndex and fileURI populated.
      // If there are cases where SourceIndex or fileURI is None, they can be ignored by presentation-compiler,
      // because they are logged.
      val allWarnings =
        extractedContractWarnings ++ extractedScriptWarnings ++ warnings

      Right((statefulContracts, statefulScripts, allWarnings))
    } catch TryUtil.catchAllThrows(workspaceErrorURI)

  private def extractWarnings(contracts: Array[CompiledCodeWrapper])(implicit logger: ClientLogger): Array[Warning] =
    contracts flatMap extractWarnings

  /**
   * Ensures all [[Warning]]s are assigned a [[SourceIndex]] and file URI.
   * Warnings that cannot be assigned are logged.
   *
   * Warnings returned by ralphc may or may not contain a [[SourceIndex]] or file URI.
   *
   * There multiple conditions for handling [[Warning]]s returned by ralphc
   *  - When a warning contained in a [[CompiledContract]] or [[CompiledScript]]
   *    has an empty [[SourceIndex]] or [[SourceIndex.fileURI]], the file information
   *    for the contract or script must be applied to the [[Warning]].
   *  - When a [[CompiledContract]] or [[CompiledScript]] contains a [[Warning]] for different file URI,
   *    that [[Warning]] must be reported at the file information contained with the [[Warning]] and not
   *    at the [[CompiledContract]]'s or [[CompiledScript]]'s file information.
   *  - Since [[Warning.sourceIndex]] is of type [[Option]], [[None]] cases must be reported
   *    so they can be resolved in Node.
   *  - Since [[SourceIndex.fileURI]] is of type [[Option]], [[None]] cases must be reported
   *    so they can be resolved in Node.
   *
   * @param contract The contract containing warnings.
   * @param logger   Message logger
   * @return Warnings with [[SourceIndex]] populated.
   */
  def extractWarnings(contract: CompiledCodeWrapper)(implicit logger: ClientLogger): Array[Warning] =
    contract.warnings.toArray flatMap {
      warning =>
        warning.sourceIndex match {
          case Some(warningSourceIndex) => // Warning has SourceIndex!
            if (warningSourceIndex.fileURI.isEmpty) {             // Is the Warning's fileURI empty?
              contract.ast.sourceIndex.flatMap(_.fileURI) match { // It's empty! Assign it from Contract's fileURI.
                case Some(contractFileURI) =>
                  // update the Warning's fileURI
                  Some(warning.copy(sourceIndex = Some(warningSourceIndex.copy(fileURI = Some(contractFileURI)))))

                case None =>
                  // Contract also does not have a fileURI. Report this as an error so it can be resolved in Node.
                  logger.error(CompilerLogMessage.UnassignedWarningNoneFileURI(warning, contract.ast.name))
                  None
              }
            } else { // Warning is good. It contains all file information.
              Some(warning)
            }

          case None => // Warning's SourceIndex is None. Assign it one, either `Contract.Ident.SourceIndex` or `Contract.SourceIndex`.
            if (contract.ast.ident.sourceIndex.isDefined) {
              // assign the Contract.Ident.SourceIndex
              Some(warning.copy(sourceIndex = contract.ast.ident.sourceIndex))
            } else if (contract.ast.sourceIndex.isDefined) {
              logger.error(CompilerLogMessage.NoneIdentSourceIndex(warning, contract.ast.name))
              // assign the Contract.SourceIndex
              Some(warning.copy(sourceIndex = contract.ast.sourceIndex))
            } else {
              // SourceIndex not set anywhere. Report this as an error so it can be resolved in Node.
              logger.error(CompilerLogMessage.UnassignedWarningNoneFileInfo(warning, contract.ast.name))
              None
            }
        }
    }

}
