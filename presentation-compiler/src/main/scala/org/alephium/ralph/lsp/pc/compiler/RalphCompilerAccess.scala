package org.alephium.ralph.lsp.pc.compiler

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript, CompilerOptions}
import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.pc.data.FileError
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralphc.Config

import java.net.URI
import scala.collection.immutable.ArraySeq

/**
 * Implements ralph parsing and compilation functions accessing the ralph compiler code.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object RalphCompilerAccess extends CompilerAccess {

  def parseCode(code: String): Either[CompilerError.FormattableError, Ast.MultiContract] =
    RalphcExtension.parseMultiContract(code)

  def compileWorkspace(workspaceState: WorkspaceState.Parsed,
                       options: CompilerOptions): WorkspaceState.Compiled = {
    val contractsToCompile = workspaceState.sourceCodeStates.flatMap(_.parsedAST.contracts)
    val astToCompile = Ast.MultiContract(contractsToCompile, None)
    val compilationResult = RalphcExtension.compileMultiContract(astToCompile, options)
    toWorkspaceState(workspaceState, compilationResult)
  }

  override def compileForDeployment(workspaceURI: URI,
                                    config: Config): WorkspaceState.Compiled = {
    val compilationResult = RalphcExtension.compileForDeployment(workspaceURI, config)
    // TODO: compileForDeployment does not use any information from presentation-compiler. It compiles from disk.
    //       This should still return a valid [[WorkspaceState.Compiled]] state.
    toWorkspaceState(
      currentState = ???,
      compilationResult = compilationResult
    )
  }

  private def toWorkspaceState(currentState: WorkspaceState.Parsed,
                               compilationResult: Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])]): WorkspaceState.Compiled =
    compilationResult match {
      case Left(workspaceError) =>
        // File or sourcePosition position information is not available for this error,
        // report it as a workspace error.
        WorkspaceState.Compiled(
          sourceCodeStates = currentState.sourceCodeStates, // SourceCode remains the same as existing state
          workspaceErrors = ArraySeq(workspaceError), // errors to report
          previousState = currentState,
        )

      case Right((compiledContracts, compiledScripts)) =>
        buildCompiledWorkspaceState(
          currentWorkspaceState = currentState,
          compiledContracts = compiledContracts,
          compiledScripts = compiledScripts
        )
    }

  private def buildCompiledWorkspaceState(currentWorkspaceState: WorkspaceState.Parsed,
                                          compiledContracts: Array[CompiledContract],
                                          compiledScripts: Array[CompiledScript]): WorkspaceState.Compiled = {
    val newSourceCodeStates =
      currentWorkspaceState.sourceCodeStates map {
        sourceCodeState =>
          val matchedCode = // Map contracts and scripts to their fileURIs.
            findMatchingContractOrScript(
              parsedAstToCompile = sourceCodeState.parsedAST,
              compiledContracts = compiledContracts,
              compiledScripts = compiledScripts
            )

          val (errors, compiledCode) =
            matchedCode.partitionMap(either => either)

          if (errors.nonEmpty) // if true, return errors
            SourceCodeState.Errored(
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
              previousState = sourceCodeState
            )

      }

    // new WorkspaceState
    WorkspaceState.Compiled(
      sourceCodeStates = newSourceCodeStates,
      workspaceErrors = ArraySeq.empty,
      previousState = currentWorkspaceState
    )
  }

  private def findMatchingContractOrScript(parsedAstToCompile: Ast.MultiContract,
                                           compiledContracts: Array[CompiledContract],
                                           compiledScripts: Array[CompiledScript]): Seq[Either[FileError, Either[CompiledContract, CompiledScript]]] =
    parsedAstToCompile.contracts map {
      contract =>
        findMatchingContractOrScript(
          contract = contract,
          compiledContracts = compiledContracts,
          compiledScripts = compiledScripts
        )
    }

  private def findMatchingContractOrScript(contract: Ast.ContractWithState,
                                           compiledContracts: Array[CompiledContract],
                                           compiledScripts: Array[CompiledScript]): Either[FileError, Either[CompiledContract, CompiledScript]] = {
    val matchingContract = findMatchingContract(contract, compiledContracts)
    val matchingScript = findMatchingScript(contract, compiledScripts)

    (matchingContract, matchingScript) match {
      case (Some(contract), Some(_)) =>
        // This is already disallowed by the ralph compiler.
        // This should never occur in reality but this needed so type checks are covered.
        val error = FileError(s"Found a contract and script with the same type name '${contract.ast.name}'")
        Left(error)

      case (Some(contract), None) =>
        Right(Left(contract))

      case (None, Some(script)) =>
        Right(Right(script))

      case (None, None) =>
        // Code submitted to compile should always return a result.
        // This should never occur in reality but this needed so type checks are covered.
        val error = FileError(s"Code '${contract.name}' not compiled.")
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
