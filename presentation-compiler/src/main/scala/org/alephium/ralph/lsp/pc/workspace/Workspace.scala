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
 *
 * All functions are all immutable. They all returns the next workspace state, given the current state.
 */
object Workspace {

  /** First state of a workspace which just knows about the workspace root folder. */
  def create(workspaceURI: URI): WorkspaceState.Initialised =
    WorkspaceState.Initialised(workspaceURI)

  /**
   * Build the workspace.
   *
   * Downgrade the state (to trigger full workspace recompilation) only if
   * this is a new build file else returns the same state.
   * */
  def build(buildURI: URI,
            build: Option[String],
            state: WorkspaceState): Either[FormattableError, Option[WorkspaceState.BuildAware]] =
    WorkspaceBuild.validateBuildURI(
      buildURI = buildURI,
      workspaceURI = state.workspaceURI
    ) flatMap {
      buildURI =>
        WorkspaceBuild.readBuild(
          buildURI = buildURI,
          code = build,
        ) map {
          newBuild =>
            state match {
              case currentState: WorkspaceState.BuildAware =>
                // if the new build-file is the same as current build-file, return the
                // same state, so a new build does not unnecessarily gets triggered.
                if (currentState.build == newBuild)
                  None
                else // else the build file has changed, start a new workspace with the new build.
                  Some(WorkspaceState.BuildCompiled(newBuild))

              case WorkspaceState.Initialised(_) =>
                // upgrade the state to build-file aware.
                Some(WorkspaceState.BuildCompiled(newBuild))
            }
        }
    }

  def build(buildURI: URI,
            build: Option[String],
            state: WorkspaceState.Initialised): Either[FormattableError, WorkspaceState.BuildCompiled] =
    WorkspaceBuild.validateBuildURI(
      buildURI = buildURI,
      workspaceURI = state.workspaceURI
    ) flatMap {
      buildURI =>
        WorkspaceBuild.readBuild(
          buildURI = buildURI,
          code = build,
        ) map WorkspaceState.BuildCompiled
    }

  /**
   * Fetches existing workspace or initialises a new one from the configured build file.
   *
   * If the build file is not configured, it returns an error.
   */
  def getOrBuild(workspace: WorkspaceState): Either[FormattableError, WorkspaceState.BuildAware] =
    workspace match {
      case built: WorkspaceState.BuildAware =>
        // Already built. Return the same state.
        Right(built)

      case state: WorkspaceState.Initialised =>
        // Build file is not compiled. Build it!
        build(
          buildURI = WorkspaceBuild.toBuildURI(state.workspaceURI),
          build = None,
          state = state
        )
    }

  /**
   * Initial workspaces collects paths to all OnDisk ralph files.
   *
   * @param state Current state of the workspace.
   * @return New workspace state which aware of all workspace source code files.
   */
  def build(state: WorkspaceState.BuildCompiled)(implicit compiler: CompilerAccess): Either[FormattableError, WorkspaceState.UnCompiled] =
    SourceCode
      .initialise(state.build.contractURI)
      .map(WorkspaceState.UnCompiled(state.build, _))

  /**
   * Parses source-code in that is not already in parsed state.
   *
   * @return A new workspace state with errors if parse fails
   *         or [[WorkspaceState.Parsed]] is returned on successful parse.
   */
  def parse(workspace: WorkspaceState.UnCompiled)(implicit compiler: CompilerAccess): WorkspaceState.SourceAware =
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

  /**
   * Parses and compiles the workspace.
   *
   * @param workspace Current workspace state
   * @return New workspace state with compilation results of all source files.
   */
  def parseAndCompile(workspace: WorkspaceState.UnCompiled)(implicit compiler: CompilerAccess): WorkspaceState.SourceAware =
    parse(workspace) match {
      case unCompiled: WorkspaceState.UnCompiled =>
        // Still un-compiled. There are errors.
        unCompiled

      case errored: WorkspaceState.Errored =>
        errored // there are still workspace level errors

      case parsed: WorkspaceState.Parsed =>
        // Successfully parsed! Compile it!
        compileParsed(parsed)

      case compiled: WorkspaceState.Compiled =>
        // State already compiled. Process it's parsed state.
        // FIXME: It might not be necessary to re-compile this state since it's already compiled.
        compileParsed(compiled.parsed)
    }

  /**
   * Compiles a parsed workspace.
   */
  def compileParsed(workspace: WorkspaceState.Parsed)(implicit compiler: CompilerAccess): WorkspaceState.CompileRun = {
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

  /**
   * Apply the code changes to the workspace state.
   *
   * @param fileURI      File updated
   * @param updatedCode  Updated code
   * @param currentState Current configured workspace state
   * @return New workspace state.
   */
  def codeChanged(fileURI: URI,
                  updatedCode: Option[String],
                  currentState: WorkspaceState.BuildAware)(implicit compilerAccess: CompilerAccess): Either[FormattableError, WorkspaceState.UnCompiled] = {
    val sourceAware =
      currentState match {
        case sourceAware: WorkspaceState.SourceAware =>
          // source code is already know
          Right(sourceAware)

        case built: WorkspaceState.BuildCompiled =>
          // This is a new build, initialise its state so it's compilable.
          build(built)
      }

    sourceAware map {
      currentState =>
        val newSourceCodeState =
          updatedCode match {
            case Some(newCode) =>
              SourceCodeState.UnCompiled(fileURI, newCode)

            case None =>
              SourceCodeState.OnDisk(fileURI)
          }

        val updatedFileStates =
          currentState.updateOrAdd(newSourceCodeState)

        WorkspaceState.UnCompiled(
          build = currentState.build,
          sourceCode = updatedFileStates
        )
    }
  }

  /**
   * Compile the code in preparation for deployment. The final step in compilation.
   *
   * The state of presentation compiler is not used here. All ralph code files are accessed from disk.
   * LSP client should ensure that all files are flushed to disk.
   *
   * @param workspaceURI Workspace path
   * @param config       Compiler configuration
   * @param compiler     Target ralph compiler
   * @return New workspace state that PresentationalCompiler can continue with.
   */
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

  private[workspace] def toWorkspaceState(currentState: WorkspaceState.Parsed,
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
