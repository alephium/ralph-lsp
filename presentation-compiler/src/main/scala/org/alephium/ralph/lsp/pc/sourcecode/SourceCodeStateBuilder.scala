// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript, Warning}
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error.StringError
import org.alephium.ralph.lsp.access.compiler.CompilerRunResult
import org.alephium.ralph.lsp.pc.sourcecode.warning.StringWarning
import org.alephium.ralph.lsp.utils.log.ClientLogger

import java.net.URI
import scala.collection.immutable.ArraySeq

private object SourceCodeStateBuilder {

  /**
   * Compilation results from `ralphc` do not retain the source file URIs or Path information.
   *
   * This function map the compilation results back to their respect file URIs,
   * relying on the assumption that all type-definitions have unique identifiers.
   *
   * @param parsedCode        The parsed code on which this compilation was executed.
   * @param compilationResult The compilation result to process.
   * @return Workspace-level error if no source-code association was found, or else the next source-code state for each source URI.
   */
  def toSourceCodeState(
      parsedCode: ArraySeq[SourceCodeState.Parsed],
      workspaceErrorURI: URI,
      compilationResult: CompilerRunResult
    )(implicit logger: ClientLogger): Either[CompilerRunResult.Errored, SourceCodeCompilerRun] =
    compilationResult match {
      case error: CompilerRunResult.Errored =>
        // update the error to SourceCodeState
        toCompilationError(
          parsedCode = parsedCode,
          error = error.error
        ) match {
          case Some(updatedSourceCode) =>
            val run =
              SourceCodeCompilerRun(
                compiledSource = updatedSourceCode,
                compilerRunGlobalState = error.globalState
              )

            Right(run)

          case None =>
            Left(error)
        }

      case compiled: CompilerRunResult.Compiled =>
        val state =
          buildCompiledSourceCodeState(
            parsedCode = parsedCode,
            compiledContracts = compiled.contracts,
            compiledScripts = compiled.scripts,
            warnings = compiled.warnings,
            workspaceErrorURI = workspaceErrorURI
          )

        val run =
          SourceCodeCompilerRun(
            compiledSource = state,
            compilerRunGlobalState = Some(compiled.globalState)
          )

        Right(run)
    }

  /**
   * If `fileURI` exists in the given compilation error, this function updates the current [[SourceCodeState]] at
   * that `fileURI` with an error state [[SourceCodeState.ErrorCompilation]] indicating that this source file errored
   * during compilation.
   *
   * @param parsedCode The parsed code used for compilation.
   * @param error      The error occurred during compilation.
   * @return An [[ArraySeq]] of [[SourceCodeState]]s if the `fileURI` is defined in the
   *         error's [[org.alephium.ralph.SourceIndex]], otherwise, [[None]].
   */
  private def toCompilationError(
      parsedCode: ArraySeq[SourceCodeState.Parsed],
      error: CompilerMessage.AnyError): Option[ArraySeq[SourceCodeState.IsParsedAndCompiled]] =
    error
      .index
      .fileURI
      .flatMap {
        fileURI => // fileURI exists
          parsedCode
            .find(_.fileURI == fileURI) // find the target file
            .map {
              parsed =>
                // Update the target file to an error state.
                val sourceError =
                  SourceCodeState.ErrorCompilation(
                    errors = ArraySeq(error),
                    parsed = parsed
                  )

                // replace the source-code state.
                parsedCode
                  .filter(_.fileURI != fileURI)
                  .appended(sourceError)
            }
      }

  /**
   * Maps compiled-code to it's parsed code.
   *
   * @param parsedCode        Parsed source-code state used to run this compilation.
   * @param compiledContracts Resulting compiled contracts.
   * @param compiledScripts   Resulting compiled scripts.
   * @return Compiled source-code state that might contain source-code level
   *         errors if a compiled source-code instance was not found its parsed state.
   */
  private def buildCompiledSourceCodeState(
      parsedCode: ArraySeq[SourceCodeState.Parsed],
      compiledContracts: Array[CompiledContract],
      compiledScripts: Array[CompiledScript],
      warnings: Array[Warning],
      workspaceErrorURI: URI
    )(implicit logger: ClientLogger): ArraySeq[SourceCodeState.IsCompiled] =
    parsedCode map {
      sourceCodeState =>
        val matchedCode = // Map contracts and scripts to their fileURIs.
          findMatchingContractOrScript(
            sourceCodeState = sourceCodeState,
            compiledContracts = compiledContracts,
            compiledScripts = compiledScripts,
            workspaceErrorURI = workspaceErrorURI
          )

        val (errors, compiledCode) =
          matchedCode partitionMap identity

        if (errors.nonEmpty) // if true, return errors
          SourceCodeState.ErrorCompilation(
            errors = errors,
            parsed = sourceCodeState
          )
        else // else, return successfully compiled
          SourceCodeState.Compiled(
            compiledCode = compiledCode,
            parsed = sourceCodeState,
            warnings = collectURIWarnings(
              fileURI = sourceCodeState.fileURI,
              warnings = warnings
            ).toSeq
          )
    }

  /**
   * Collects all warnings associated with the given file URI.
   *
   * @param fileURI  The file URI for which warnings are being collected.
   * @param warnings Warnings to filter and search.
   * @return A collection of [[StringWarning]]s for the given file URI.
   */
  private def collectURIWarnings(
      fileURI: URI,
      warnings: Array[Warning]
    )(implicit logger: ClientLogger): Array[StringWarning] =
    warnings // collect warnings to report at the given `fileURI`, that were returned by `genStatefulContracts()`
      .filter {
        warning =>
          warning.sourceIndex.exists(_.fileURI contains fileURI)
      }
      .map {
        warning =>
          StringWarning(
            warning = warning,
            fileURI = fileURI
          )
      }

  private def findMatchingContractOrScript(
      sourceCodeState: SourceCodeState.Parsed,
      compiledContracts: Array[CompiledContract],
      compiledScripts: Array[CompiledScript],
      workspaceErrorURI: URI): Seq[Either[StringError, Either[CompiledContract, CompiledScript]]] =
    sourceCodeState
      .astStrict
      .statements
      .collect {
        case statement: Tree.Source => // collect only the source-code, ignoring import statements.
          statement.ast
      }
      .collect {
        // Only Contracts and Scripts can be compiled
        case contract: Ast.Contract if !contract.isAbstract =>
          contract

        case script: Ast.TxScript =>
          script
      }
      .map {
        contract =>
          findMatchingContractOrScript(
            contract = contract,
            compiledContracts = compiledContracts,
            compiledScripts = compiledScripts,
            workspaceErrorURI = workspaceErrorURI
          )
      }

  private def findMatchingContractOrScript(
      contract: Ast.ContractWithState,
      compiledContracts: Array[CompiledContract],
      compiledScripts: Array[CompiledScript],
      workspaceErrorURI: URI): Either[StringError, Either[CompiledContract, CompiledScript]] = {
    val matchingContract = findMatchingContract(contract, compiledContracts)
    val matchingScript   = findMatchingScript(contract, compiledScripts)

    (matchingContract, matchingScript) match {
      case (Some(contract), Some(_)) =>
        // This is already disallowed by the ralph compiler.
        // This should never occur in reality but this is needed so type checks are covered.
        val error =
          StringError(
            message = s"Found a contract and script with the duplicate type name '${contract.ast.name}'",
            fileURI = workspaceErrorURI
          )

        Left(error)

      case (Some(contract), None) =>
        Right(Left(contract))

      case (None, Some(script)) =>
        Right(Right(script))

      case (None, None) =>
        // Code submitted to compile should always return a result.
        // This should never occur in reality but this is needed so type checks are covered.
        val error =
          StringError(
            message = s"Code '${contract.name}' not compiled.",
            fileURI = workspaceErrorURI
          )

        Left(error)
    }
  }

  private def findMatchingContract(
      contractsToCompile: Ast.ContractWithState,
      compiledContracts: Array[CompiledContract]): Option[CompiledContract] =
    compiledContracts.find(_.ast.name == contractsToCompile.name)

  private def findMatchingScript(
      contractsToCompile: Ast.ContractWithState,
      compiledScripts: Array[CompiledScript]): Option[CompiledScript] =
    compiledScripts.find(_.ast.name == contractsToCompile.name)

}
