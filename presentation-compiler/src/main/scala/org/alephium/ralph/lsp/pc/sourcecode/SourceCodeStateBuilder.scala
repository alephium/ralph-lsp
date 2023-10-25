package org.alephium.ralph.lsp.pc.sourcecode
import scala.collection.immutable.ListMap

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript}
import org.alephium.ralph.lsp.access.compiler.message.error.StringError
import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

import scala.collection.immutable.ArraySeq

private[sourcecode] object SourceCodeStateBuilder {

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
  def toSourceCodeState(parsedCode: ArraySeq[SourceCodeState.Parsed],
                        compilationResult: Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])]): Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState.CodeAware]] =
    compilationResult map {
      case (compiledContracts, compiledScripts) =>
        buildCompiledSourceCodeState(
          parsedCode = parsedCode,
          compiledContracts = compiledContracts,
          compiledScripts = compiledScripts
        )
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
  private def buildCompiledSourceCodeState(parsedCode: ArraySeq[SourceCodeState.Parsed],
                                           compiledContracts: Array[CompiledContract],
                                           compiledScripts: Array[CompiledScript]): ArraySeq[SourceCodeState.CodeAware] =
    parsedCode map {
      sourceCodeState =>
        val matchedCode = // Map contracts and scripts to their fileURIs.
          findMatchingContractOrScript(
            parsedContracts = sourceCodeState.contracts,
            compiledContracts = compiledContracts,
            compiledScripts = compiledScripts
          )

        val (errors, compiledCode) =
          matchedCode.partitionMap(either => either)

        if (errors.nonEmpty) // if true, return errors
          SourceCodeState.ErrorSource(
            fileURI = sourceCodeState.fileURI,
            code = sourceCodeState.code,
            errors = errors,
            previous = Some(sourceCodeState)
          )
        else // else, return successfully compiled
          SourceCodeState.Compiled(
            fileURI = sourceCodeState.fileURI,
            code = sourceCodeState.code,
            compiledCode = compiledCode,
            parsed = sourceCodeState
          )
    }

  private def findMatchingContractOrScript(parsedContracts: Seq[ContractWithState],
                                           compiledContracts: Array[CompiledContract],
                                           compiledScripts: Array[CompiledScript]): Seq[Either[StringError, Either[CompiledContract, CompiledScript]]] =
    parsedContracts
     .collect { case c: Ast.Contract if !c.isAbstract => c } //Only contracts can be compiled
     .map {
      contract =>
        findMatchingContractOrScript(
          contract = contract,
          compiledContracts = compiledContracts,
          compiledScripts = compiledScripts
        )
    }

  private def findMatchingContractOrScript(contract: Ast.ContractWithState,
                                           compiledContracts: Array[CompiledContract],
                                           compiledScripts: Array[CompiledScript]): Either[StringError, Either[CompiledContract, CompiledScript]] = {
    val matchingContract = findMatchingContract(contract, compiledContracts)
    val matchingScript = findMatchingScript(contract, compiledScripts)

    (matchingContract, matchingScript) match {
      case (Some(contract), Some(_)) =>
        // This is already disallowed by the ralph compiler.
        // This should never occur in reality but this is needed so type checks are covered.
        val error = StringError(s"Found a contract and script with the duplicate type name '${contract.ast.name}'")
        Left(error)

      case (Some(contract), None) =>
        Right(Left(contract))

      case (None, Some(script)) =>
        Right(Right(script))

      case (None, None) =>
        // Code submitted to compile should always return a result.
        // This should never occur in reality but this is needed so type checks are covered.
        val error = StringError(s"Code '${contract.name}' not compiled.")
        Left(error)
    }
  }

  private def findMatchingContract(contractsToCompile: Ast.ContractWithState,
                                   compiledContracts: Array[CompiledContract]): Option[CompiledContract] =
    compiledContracts.find(_.ast.name == contractsToCompile.name)

  private def findMatchingScript(contractsToCompile: Ast.ContractWithState,
                                 compiledScripts: Array[CompiledScript]): Option[CompiledScript] =
    compiledScripts.find(_.ast.name == contractsToCompile.name)

}
