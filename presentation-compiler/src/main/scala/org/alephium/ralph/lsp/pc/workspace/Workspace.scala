package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.error.{FileError, WorkspaceError}
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralphc.Config

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.util.{Failure, Success}

/**
 * Implements functions operating on all source-code files within a workspace.
 */
private[pc] object Workspace {

  def configure(state: WorkspaceState.UnConfigured): Either[FormattableError, WorkspaceConfig] =
    WorkspaceConfig.readWorkspaceConfig(state.workspaceURI) match {
      case Failure(exception) =>
        scribe.error(exception)
        Left(WorkspaceError(exception.getMessage))

      case Success(config) =>
        Right(config)
    }

  def initialise(state: WorkspaceState.UnConfigured)(implicit compiler: CompilerAccess): Either[FormattableError, WorkspaceState.UnCompiled] =
    configure(state)
      .flatMap(initialise)

  /**
   * Initialise a workspace for the given workspace config.
   *
   * @return URIs of all source-code files returned by the compiler.
   * */
  def initialise(config: WorkspaceConfig)(implicit compiler: CompilerAccess): Either[FormattableError, WorkspaceState.UnCompiled] =
    SourceCode
      .initialise(config.contractURI)
      .map(WorkspaceState.UnCompiled(config, _))

  /**
   * Parses source-code in that is not already in parsed state.
   *
   * @return A new workspace state with errors if parse fails
   *         or [[WorkspaceState.Parsed]] is returned on successful parse.
   */
  def parse(wsState: WorkspaceState.UnCompiled)(implicit compiler: CompilerAccess): WorkspaceState.Configured =
    if (wsState.sourceCode.isEmpty) {
      wsState
    } else {
      // Parse all source code. TODO: Could be concurrent.
      val triedParsedStates =
        wsState.sourceCode.map(SourceCode.parse)

      // collect all parsed code
      val actualParsedStates =
        triedParsedStates.collect {
          case state: SourceCodeState.Parsed =>
            state

          case code: SourceCodeState.Compiled =>
            code.previousState
        }

      // if there is a difference in size then there are error states in the workspace.
      if (actualParsedStates.size != triedParsedStates.size)
        WorkspaceState.UnCompiled(wsState.config, triedParsedStates)
      else // Successfully parsed and can be moved onto compilation process.
        WorkspaceState.Parsed(wsState.config, actualParsedStates)
    }

  /**
   * Compiles source-code which is already parsed. Does not attempt to parse any code.
   */
  def compileParsed(wsState: WorkspaceState)(implicit compiler: CompilerAccess): WorkspaceState.Configured =
    wsState match {
      case state: WorkspaceState.UnCompiled =>
        state

      case state: WorkspaceState.Compiled =>
        state

      case state: WorkspaceState.Parsed =>
        val contractsToCompile =
          state.sourceCode.flatMap(_.contracts)

        val compilationResult =
          compiler.compileContracts(
            contracts = contractsToCompile,
            options = state.config.ralphcConfig.compilerOptions
          )

        toWorkspaceState(
          currentState = state,
          compilationResult = compilationResult
        )
    }

  def parseAndCompile(wsState: WorkspaceState.Configured)(implicit compiler: CompilerAccess): WorkspaceState.Configured = {
    //    val parsed = Workspace.parse(wsState)
    //    Workspace.compileParsed(parsed)
    ???
  }

  def parseAndCompile(wsState: WorkspaceState.UnCompiled)(implicit compiler: CompilerAccess): WorkspaceState.Configured = {
    val parsed = Workspace.parse(wsState)
    Workspace.compileParsed(parsed)
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
          sourceCode = currentState.sourceCode, // SourceCode remains the same as existing state
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
      currentWorkspaceState.sourceCode map {
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
      sourceCode = newSourceCodeStates,
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
