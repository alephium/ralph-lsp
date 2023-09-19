package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.{Ast, CompiledContract, CompiledScript}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.error.StringMessage
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralphc.Config

import java.net.URI
import scala.collection.immutable.ArraySeq

/**
 * Implements functions operating on all source-code files within a workspace.
 */
private[pc] object Workspace {

  def buildChanged(buildURI: URI,
                   build: Option[String],
                   state: WorkspaceState): Either[FormattableError, WorkspaceState] =
    WorkspaceBuild.readBuild(
      buildURI = buildURI,
      code = build,
    ) map {
      newBuild =>
        // if the new build-file is the same as current build-file, return the same state.
        if (state.buildOpt.contains(newBuild))
          state
        else // else build file has changed, start a new workspace with the new build.
          WorkspaceState.Built(newBuild)
    }

  def initialise(state: WorkspaceState.Built)(implicit compiler: CompilerAccess): Either[FormattableError, WorkspaceState.UnCompiled] =
    initialise(state.build)

  /**
   * Initialise a workspace for the given build file.
   *
   * @return URIs of all source-code files returned by the compiler.
   * */
  def initialise(build: WorkspaceBuild)(implicit compiler: CompilerAccess): Either[FormattableError, WorkspaceState.UnCompiled] =
    SourceCode
      .initialise(build.contractURI)
      .map(WorkspaceState.UnCompiled(build, _))

  /**
   * Parses source-code in that is not already in parsed state.
   *
   * @return A new workspace state with errors if parse fails
   *         or [[WorkspaceState.Parsed]] is returned on successful parse.
   */
  def parse(workspace: WorkspaceState.UnCompiled)(implicit compiler: CompilerAccess): WorkspaceState.Configured =
    if (workspace.sourceCode.isEmpty) {
      workspace
    } else {
      // Parse all source code. TODO: Could be concurrent.
      val triedParsedStates =
        workspace.sourceCode.map(SourceCode.parse)

      // collect all parsed code
      val actualParsedStates =
        triedParsedStates.collect {
          case state: SourceCodeState.Parsed =>
            state

          case code: SourceCodeState.Compiled =>
            code.parsed
        }

      // if there is a difference in size then there are error states in the workspace.
      if (actualParsedStates.size != triedParsedStates.size)
        WorkspaceState.UnCompiled(workspace.build, triedParsedStates)
      else // Successfully parsed and can be moved onto compilation process.
        WorkspaceState.Parsed(workspace.build, actualParsedStates)
    }

  /** Triggers parse and compile in sequence for a configured workspace. */
  def parseAndCompile(workspace: WorkspaceState.Configured)(implicit compiler: CompilerAccess): WorkspaceState.Configured = {
    // try parsing the input workspace
    val parseTried =
      workspace match {
        case unCompiled: WorkspaceState.UnCompiled =>
          parse(unCompiled)

        case errored: WorkspaceState.Errored =>
          errored // there are existing workspace level errors

        case parsed: WorkspaceState.Parsed =>
          parsed

        case compiled: WorkspaceState.Compiled =>
          compiled.parsed
      }

    // try compile
    parseTried match {
      case unCompiled: WorkspaceState.UnCompiled =>
        // Still un-compiled. There are errors.
        unCompiled

      case errored: WorkspaceState.Errored =>
        errored // there are still workspace level errors

      case parsed: WorkspaceState.Parsed =>
        compile(parsed)

      case compiled: WorkspaceState.Compiled =>
        compile(compiled.parsed)
    }
  }

  /**
   * Compiles a parsed workspace.
   */
  def compile(workspace: WorkspaceState.Parsed)(implicit compiler: CompilerAccess): WorkspaceState.CompileRun = {
    val contractsToCompile =
      workspace.sourceCode.flatMap(_.contracts)

    val compilationResult =
      compiler.compileContracts(
        contracts = contractsToCompile,
        options = workspace.build.config.compilerOptions
      )

    toWorkspaceState(
      currentState = workspace,
      compilationResult = compilationResult
    )
  }

  def compileForDeployment(workspaceURI: URI,
                           config: Config)(implicit compiler: CompilerAccess): WorkspaceState.CompileRun = {
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

  def toWorkspaceState(currentState: WorkspaceState.Parsed,
                       compilationResult: Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])]): WorkspaceState.CompileRun =
    compilationResult match {
      case Left(workspaceError) =>
        // File or sourcePosition position information is not available for this error,
        // report it at project error.
        WorkspaceState.Errored(
          sourceCode = currentState.sourceCode, // SourceCode remains the same as existing state
          workspaceErrors = ArraySeq(workspaceError), // errors to report
          parsed = currentState,
        )

      case Right((compiledContracts, compiledScripts)) =>
        buildCompiledWorkspaceState(
          currentWorkspaceState = currentState,
          compiledContracts = compiledContracts,
          compiledScripts = compiledScripts
        )
    }

  /**
   * Maps compiled-code to it's parsed code.
   *
   * @param currentWorkspaceState Parsed state of the workspace used to run this compilation.
   * @param compiledContracts     Resulting compiled contracts.
   * @param compiledScripts       Resulting compiled scripts.
   * @return Compiled workspace state that might contain source-code level
   *         errors if a compiled source-code instance was not found its parsed state.
   */
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

    // new WorkspaceState
    WorkspaceState.Compiled(
      sourceCode = newSourceCodeStates,
      parsed = currentWorkspaceState
    )
  }

  private def findMatchingContractOrScript(parsedContracts: Seq[ContractWithState],
                                           compiledContracts: Array[CompiledContract],
                                           compiledScripts: Array[CompiledScript]): Seq[Either[StringMessage, Either[CompiledContract, CompiledScript]]] =
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
                                           compiledScripts: Array[CompiledScript]): Either[StringMessage, Either[CompiledContract, CompiledScript]] = {
    val matchingContract = findMatchingContract(contract, compiledContracts)
    val matchingScript = findMatchingScript(contract, compiledScripts)

    (matchingContract, matchingScript) match {
      case (Some(contract), Some(_)) =>
        // This is already disallowed by the ralph compiler.
        // This should never occur in reality but this needed so type checks are covered.
        val error = StringMessage(s"Found a contract and script with the duplicate type name '${contract.ast.name}'")
        Left(error)

      case (Some(contract), None) =>
        Right(Left(contract))

      case (None, Some(script)) =>
        Right(Right(script))

      case (None, None) =>
        // Code submitted to compile should always return a result.
        // This should never occur in reality but this needed so type checks are covered.
        val error = StringMessage(s"Code '${contract.name}' not compiled.")
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
