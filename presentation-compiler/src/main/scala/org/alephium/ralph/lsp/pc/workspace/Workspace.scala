package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.util.CollectionUtil._
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, BuildValidator}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState.BuildCompiled

import java.net.URI
import scala.collection.immutable.ArraySeq

/**
 * Implements functions operating on all source-code files within a workspace.
 *
 * All functions are all immutable. They all returns the next workspace state, given the current state.
 */
object Workspace {

  /** First state of a workspace which just knows about the workspace root folder. */
  def create(workspaceURI: URI): WorkspaceState.Created =
    WorkspaceState.Created(workspaceURI)

  /** Creates an un-compiled workspace for a successful build file. */
  def initialise(state: BuildState.CompileResult)(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.UnCompiled] =
    state match {
      case compiled: BuildState.BuildCompiled =>
        // Build file changed. Update the workspace and request a full workspace build.
        initialise(compiled)

      case errored: BuildState.BuildErrored =>
        Left(errored)
    }

  /**
   * Initial workspaces collects paths to all OnDisk ralph files.
   *
   * @param state Current state of the workspace.
   * @return New workspace state which aware of all workspace source code files.
   */
  def initialise(state: BuildCompiled)(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.UnCompiled] =
    SourceCode.initialise(state.contractURI) match {
      case Left(error) =>
        val buildError =
          BuildState.BuildErrored(
            buildURI = state.buildURI,
            code = Some(state.code),
            errors = ArraySeq(error)
          )

        Left(buildError)

      case Right(sourceCode) =>
        val unCompiled =
          WorkspaceState.UnCompiled(
            build = state,
            sourceCode = sourceCode
          )

        Right(unCompiled)
    }

  /**
   * Returns existing workspace or initialises a new one from the configured build file.
   * Or else reports any workspace issues.
   *
   * @note Does not update the current state. The caller should set the new state.
   */
  def initialise(workspace: WorkspaceState)(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.SourceAware] =
    workspace match {
      case aware: WorkspaceState.SourceAware =>
        Right(aware) // already initialised

      case initialised: WorkspaceState.Created =>
        val newBuild =
          Build.parseAndCompile(
            buildURI = initialised.buildURI,
            code = None,
          )

        initialise(newBuild)
    }

  /**
   * Build the workspace.
   *
   * Downgrade the state (to trigger full workspace recompilation) only if
   * this is a new build file else returns the same state.
   * */
  def build(buildURI: URI,
            code: Option[String],
            state: WorkspaceState)(implicit file: FileAccess): Option[BuildState.CompileResult] =
    BuildValidator.validateBuildURI(
      buildURI = buildURI,
      workspaceURI = state.workspaceURI
    ) match {
      case Left(error) =>
        val buildError =
          BuildState.BuildErrored(
            buildURI = buildURI,
            code = code,
            errors = ArraySeq(error)
          )

        Some(buildError)

      case Right(buildURI) =>
        Build.parseAndCompile(
          buildURI = buildURI,
          code = code,
        ) match {
          case newBuild: BuildState.BuildCompiled =>
            state match {
              case currentState: WorkspaceState.SourceAware =>
                // if the new build-file is the same as current build-file, return it as
                // no-state-changed, so that a new build does not unnecessarily gets triggered.
                if (currentState.build == newBuild)
                  None
                else // else the build file has changed, return the new build.
                  Some(newBuild)

              case WorkspaceState.Created(_) =>
                // upgrade the state to build-file aware.
                Some(newBuild)
            }

          case errored: BuildState.BuildErrored =>
            Some(errored)
        }
    }

  /**
   * Parses source-code in that is not already in parsed state.
   *
   * @return A new workspace state with errors if parse fails
   *         or [[WorkspaceState.Parsed]] is returned on successful parse.
   */
  def parse(workspace: WorkspaceState.UnCompiled)(implicit file: FileAccess,
                                                  compiler: CompilerAccess): WorkspaceState.SourceAware =
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
  def parseAndCompile(workspace: WorkspaceState.UnCompiled)(implicit file: FileAccess,
                                                            compiler: CompilerAccess): WorkspaceState.SourceAware =
    parse(workspace) match {
      case unCompiled: WorkspaceState.UnCompiled =>
        // Still un-compiled. There are errors.
        unCompiled

      case errored: WorkspaceState.Errored =>
        errored // there are still workspace level errors

      case parsed: WorkspaceState.Parsed =>
        // Successfully parsed! Compile it!
        compile(parsed)

      case compiled: WorkspaceState.Compiled =>
        // State already compiled. Process it's parsed state.
        // FIXME: It might not be necessary to re-compile this state since it's already compiled.
        compile(compiled.parsed)
    }

  /**
   * Compiles a parsed workspace.
   */
  def compile(workspace: WorkspaceState.Parsed)(implicit compiler: CompilerAccess): WorkspaceState.CompilerRun = {
    val compilationResult =
      SourceCode.compile(
        sourceCode = workspace.sourceCode,
        compilerOptions = workspace.build.config.compilerOptions
      )

    WorkspaceStateBuilder.toWorkspaceState(
      currentState = workspace,
      compilationResult = compilationResult
    )
  }

  /**
   * Processes source or build file change.
   *
   * @param fileURI File that changed.
   * @param code    Content of the file.
   */
  def changed(fileURI: URI,
              code: Option[String],
              currentWorkspace: WorkspaceState)(implicit file: FileAccess,
                                                compiler: CompilerAccess): Option[WorkspaceChangeResult] = {
    val fileExtension =
      URIUtil.getFileExtension(fileURI)

    if (fileExtension == Build.BUILD_FILE_EXTENSION) {
      // process build change
      val result =
        Workspace.buildChanged(
          fileURI = fileURI,
          code = code,
          workspace = currentWorkspace
        )

      Some(WorkspaceChangeResult.BuildChanged(result))
    } else if (fileExtension == CompilerAccess.RALPH_FILE_EXTENSION) {
      // process source code change
      val sourceResult =
        Workspace.sourceCodeChanged(
          fileURI = fileURI,
          updatedCode = code,
          workspace = currentWorkspace
        )

      Some(WorkspaceChangeResult.SourceChanged(sourceResult))
    } else {
      None
    }
  }

  /**
   * Handles changes to the build valid.
   *
   * If the build file is valid, this drops existing compilations
   * and starts a fresh workspace.
   *
   * @param fileURI Location of the build file.
   * @param code    Build file's content.
   */
  def buildChanged(fileURI: URI,
                   code: Option[String],
                   workspace: WorkspaceState)(implicit file: FileAccess,
                                              compiler: CompilerAccess): Option[Either[BuildState.BuildErrored, WorkspaceState]] =
    if (workspace.buildURI.resolve(fileURI) == workspace.buildURI) // Check: Is this fileURI an updated version of the current workspace build
      Workspace.build(
        buildURI = fileURI,
        code = code,
        state = workspace
      ) match {
        case Some(newBuild) =>
          // this is a new build. initialise a fresh build.
          val result =
            initialise(newBuild) map parseAndCompile

          Some(result)

        case None =>
          // no build change occurred, using existing workspace.
          Some(Right(workspace))
      }
    else
      None // ignore file that is not in the root workspace directory

  /**
   * Handles changes to ralph code files.
   *
   * @param fileURI     Location of the source file.
   * @param updatedCode Source changes
   */
  def sourceCodeChanged(fileURI: URI,
                        updatedCode: Option[String],
                        workspace: WorkspaceState)(implicit file: FileAccess,
                                                   compiler: CompilerAccess): Either[BuildState.BuildErrored, WorkspaceState.SourceAware] =
    initialise(workspace) map {
      initialised =>
        if (URIUtil.isChild(initialised.build.contractURI, fileURI)) {
          // source belongs to this workspace, process compilation including this file's changed code.
          val newSourceCode =
            downgradeSourceState(
              fileURI = fileURI,
              updatedCode = updatedCode,
              sourceCode = initialised.sourceCode
            )

          // create new un-compiled workspace.
          val unCompiledWorkspace =
            WorkspaceState.UnCompiled(
              build = initialised.build,
              sourceCode = newSourceCode
            )

          // parse and compile the new state.
          Workspace.parseAndCompile(unCompiledWorkspace)
        } else {
          // file does not belong to this workspace, do not compile it and return initialised workspace.
          initialised
        }
    }

  /**
   * Downgrades the current state of updated source-code so it gets re-parsed and re-compiled.
   * Also checks if the file is deleted so it could be removed from compilation.
   *
   * @param fileURI     Updated code's file-location
   * @param updatedCode The updated code
   * @param sourceCode  Existing source-code
   * @return New source code with applied change.
   */
  def downgradeSourceState(fileURI: URI,
                           updatedCode: Option[String],
                           sourceCode: ArraySeq[SourceCodeState])(implicit file: FileAccess): ArraySeq[SourceCodeState] =
    updatedCode match {
      case Some(newCode) =>
        // new source code, store it as un-compiled.
        val newState =
          SourceCodeState.UnCompiled(fileURI, newCode)

        // update or add it to the existing collection
        updateOrAdd(
          collection = sourceCode,
          update = newState
        )

      case None =>
        // no source code sent from client, check it still exists.
        file.exists(fileURI) match {
          case Left(error) =>
            // failed to check
            val newState =
              SourceCodeState.ErrorAccess(
                fileURI = fileURI,
                error = error
              )

            updateOrAdd(
              collection = sourceCode,
              update = newState
            )

          case Right(exists) =>
            if (exists) {
              // source-code exists, set it as on-disk so it gets read during the next parse & compilation.
              val newState =
                SourceCodeState.OnDisk(fileURI)

              updateOrAdd(
                collection = sourceCode,
                update = newState
              )
            } else {
              // file does not exist, remove it.
              sourceCode.filter(_.fileURI != fileURI)
            }
        }
    }
}
