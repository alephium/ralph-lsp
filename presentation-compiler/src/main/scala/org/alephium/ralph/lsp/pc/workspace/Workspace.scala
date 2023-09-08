package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript, CompilerOptions}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.error.FileError
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralphc.Config

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.util.Try

/**
 * Implements functions operating on all source-code files within a workspace.
 */
private[pc] object Workspace {

  def initialise(config: Config)(implicit compiler: CompilerAccess): Try[WorkspaceState.UnCompiled] =
    SourceCode
      .initialise(config.contractPath)
      .map(WorkspaceState.UnCompiled(_))

  def parseAndCompile(wsState: WorkspaceState.UnCompiled,
                      compilerOptions: CompilerOptions)(implicit compiler: CompilerAccess): WorkspaceState = {
    val parsed = Workspace.parse(wsState)
    Workspace.compileParsed(parsed, compilerOptions)
  }

  def parse(wsState: WorkspaceState.UnCompiled)(implicit compiler: CompilerAccess): WorkspaceState = {
    val triedParsedStates =
      wsState.sourceCodeStates.map(SourceCode.parse)

    val actualParsedStates =
      triedParsedStates.collect {
        case state: SourceCodeState.Parsed =>
          state

        case code: SourceCodeState.Compiled =>
          code.previousState
      }

    if (actualParsedStates.size != triedParsedStates.size)
      WorkspaceState.UnCompiled(triedParsedStates)
    else
      WorkspaceState.Parsed(actualParsedStates)
  }

  def compileParsed(wsState: WorkspaceState,
                    compilerOptions: CompilerOptions)(implicit compiler: CompilerAccess): WorkspaceState =
    wsState match {
      case state: WorkspaceState.UnCompiled =>
        state

      case state: WorkspaceState.Compiled =>
        state

      case state: WorkspaceState.Parsed =>
        val contractsToCompile = state.sourceCodeStates.flatMap(_.contracts)
        val compilationResult = compiler.compileContracts(contractsToCompile, compilerOptions)
        toWorkspaceState(
          currentState = state,
          compilationResult = compilationResult
        )
    }

  def compileForDeployment(workspaceURI: URI,
                           config: Config)(implicit compiler: CompilerAccess): WorkspaceState.Compiled = {
    val result =
      compiler.compileForDeployment(
        workspaceURI = workspaceURI,
        config = config
      )

    toWorkspaceState(
      currentState = ???,
      compilationResult = result
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
              parsedContracts = sourceCodeState.contracts,
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

  private def findMatchingContractOrScript(parsedContracts: Seq[ContractWithState],
                                           compiledContracts: Array[CompiledContract],
                                           compiledScripts: Array[CompiledScript]): Seq[Either[FileError, Either[CompiledContract, CompiledScript]]] =
    parsedContracts map {
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
