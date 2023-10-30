package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.util.CollectionUtil._
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, BuildValidator}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState.BuildCompiled
import org.alephium.ralph.lsp.pc.sourcecode.imports.ImportHandler
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorUnknownFileType

import java.net.URI
import scala.collection.immutable.ArraySeq

/**
 * Implements functions operating on all source-code files within a workspace.
 *
 * All functions are all immutable. They all returns the next workspace state, given the current state.
 */
object Workspace {

  /** First stage of a workspace where just the root workspace folder is known */
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
  def initialise(state: BuildState.BuildCompiled)(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.UnCompiled] =
    SourceCode.initialise(state.contractURI) match {
      case Left(error) =>
        val buildError =
          BuildState.BuildErrored(
            buildURI = state.buildURI,
            code = Some(state.code),
            errors = ArraySeq(error),
            activateWorkspace = None
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
   * Or else reports all workspace issues.
   *
   * @note Does not update the current state. The caller should set the new state.
   */
  def build(workspace: WorkspaceState)(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.SourceAware] =
    workspace match {
      case workspace: WorkspaceState.SourceAware =>
        Right(workspace) // already initialised

      case workspace: WorkspaceState.Created =>
        val newBuild =
          Build.parseAndCompile(
            buildURI = workspace.buildURI,
            code = None,
            currentBuild = None
          )

        initialise(newBuild)
    }

  /** Build a created workspace */
  def build(buildURI: URI,
            code: Option[String],
            workspace: WorkspaceState.Created)(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.UnCompiled] =
    BuildValidator.validateBuildURI(
      buildURI = buildURI,
      workspaceURI = workspace.workspaceURI
    ) match {
      case Left(error) =>
        val buildError =
          BuildState.BuildErrored(
            buildURI = buildURI,
            code = code,
            errors = ArraySeq(error),
            activateWorkspace = None
          )

        Left(buildError)

      case Right(buildURI) =>
        val build =
          Build.parseAndCompile(
            buildURI = buildURI,
            code = code,
            currentBuild = None
          )

        initialise(build)
    }

  /**
   * If already built, return existing workspace or else invoke build.
   *
   * @param code File that changed.
   * @return Returns diagnostics in-case were build errors, or-else returns an initialised workspace.
   */
  def build(code: Option[WorkspaceFile],
            workspace: WorkspaceState)(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.SourceAware] =
    workspace match {
      case sourceAware: WorkspaceState.SourceAware =>
        // already built
        Right(sourceAware)

      case currentWorkspace: WorkspaceState.Created =>
        // workspace is created but it's not built yet. Let's build it!
        code match {
          case Some(code) if code.fileURI == currentWorkspace.buildURI =>
            // this request is for the build file, build it using the code sent by the client
            Workspace.build(
              buildURI = code.fileURI,
              code = code.text,
              workspace = currentWorkspace
            )

          case _ =>
            // else build from disk
            Workspace.build(currentWorkspace)
        }
    }

  def cleanBuild(newBuild: BuildState.CompileResult,
                 currentBuild: BuildState.BuildCompiled,
                 sourceCode: ArraySeq[SourceCodeState])(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.UnCompiled] =
    newBuild match {
      case newBuild: BuildCompiled =>
        // Build compiled OK! Compile new source-files with this new build file
        val syncedCode =
          SourceCode.synchronise(
            sourceDirectory = newBuild.contractURI,
            sourceCode = sourceCode
          )

        syncedCode match {
          case Left(error) =>
            val buildErrored =
              BuildState.BuildErrored(
                buildURI = newBuild.buildURI,
                code = Some(newBuild.code),
                errors = ArraySeq(error),
                activateWorkspace = None
              )

            Left(buildErrored)

          case Right(syncedCode) =>
            val newWorkspace =
              WorkspaceState.UnCompiled(
                build = newBuild,
                sourceCode = syncedCode
              )

            Right(newWorkspace)
        }

      case errored: BuildState.BuildErrored =>
        // Build not OK!
        val newWorkspace =
          WorkspaceState.UnCompiled(
            build = currentBuild,
            sourceCode = sourceCode
          )

        // update the error state with the new workspace to activate.
        val newError =
          errored.copy(activateWorkspace = Some(newWorkspace))

        // Build not OK! Report the error, also clear all previous diagnostics
        Left(newError)
    }

  /**
   * Parse source-code in that is not already in parsed state.
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
   * Parse and compile the workspace.
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
   * Compile a parsed workspace.
   */
  def compile(workspace: WorkspaceState.Parsed)(implicit compiler: CompilerAccess): WorkspaceState.CompilerRun = {
    val compilationResult =
      SourceCode.compile(
        sourceCode = workspace.sourceCode,
        compilerOptions = workspace.build.config.compilerOptions,
        buildDependencies = workspace.build.dependencies
      )

    WorkspaceStateBuilder.toWorkspaceState(
      currentState = workspace,
      compilationResult = compilationResult
    )
  }

  /**
   * Re-compile the entire workspace with new build-code and source-code.
   *
   * @param buildCode  New build-code
   * @param sourceCode New source-code
   * @param workspace  Current workspace
   * @return This function will always re-compile the build,
   *         so the output is always [[WorkspaceChangeResult.BuildChanged]].
   */
  def cleanCompile(buildCode: Option[String],
                   sourceCode: ArraySeq[SourceCodeState],
                   workspace: WorkspaceState.SourceAware)(implicit file: FileAccess,
                                                          compiler: CompilerAccess): WorkspaceChangeResult.BuildChanged = {
    // re-build the build file
    val buildResult =
      Build.parseAndCompile(
        buildURI = workspace.buildURI,
        code = buildCode,
        currentBuild = workspace.build
      )

    val cleanBuildResult =
      buildResult match {
        case Some(buildResult) =>
          cleanBuild(
            newBuild = buildResult,
            currentBuild = workspace.build,
            sourceCode = sourceCode
          )

        case None =>
          // No build change occurred. Process the new source-code with exiting build.
          val newWorkspace =
            WorkspaceState.UnCompiled(
              build = workspace.build,
              sourceCode = sourceCode
            )

          Right(newWorkspace)
      }

    cleanBuildResult match {
      case Left(error) =>
        WorkspaceChangeResult.BuildChanged(Some(Left(error)))

      case Right(newWorkspace) =>
        // compile new source-code
        val compiledWorkspace =
          Workspace.parseAndCompile(newWorkspace)

        // let the caller know that the build was also processed.
        WorkspaceChangeResult.BuildChanged(Some(Right(compiledWorkspace)))
    }
  }

  /**
   * Process the following:
   * - Deleted source files and folders
   * - Created source files
   *
   * @param events      Events to process
   * @param buildErrors Current build errors
   * @param workspace   Current workspace state
   * @return Workspace change result
   */
  def deleteOrCreate(events: ArraySeq[WorkspaceFileEvent],
                     buildErrors: Option[BuildState.BuildErrored],
                     workspace: WorkspaceState.SourceAware)(implicit file: FileAccess,
                                                            compiler: CompilerAccess): WorkspaceChangeResult.BuildChanged = {
    // collect events that belong to this workspace
    val workspaceEvents =
      events filter {
        event =>
          URIUtil.contains(workspace.workspaceURI, event.uri)
      }

    // is the build deleted?
    val isBuildDeleted =
      workspaceEvents contains WorkspaceFileEvent.Deleted(workspace.buildURI)

    val buildCode =
      if (isBuildDeleted) // if build is deleted, compile from disk
        None
      else // build exists, process from memory
        buildErrors match {
          case Some(errored) =>
            // use the code from the previous build's compilation run
            errored.code

          case None =>
            // previous build was good, use the compiled build code
            Some(workspace.build.code)
        }

    // apply events to the workspace
    val newSourceCode =
      Workspace.applyEvents(
        events = workspaceEvents,
        workspace = workspace
      )

    Workspace.cleanCompile(
      buildCode = buildCode,
      sourceCode = newSourceCode,
      workspace = workspace
    )
  }

  /**
   * Apply events to workspace source-code.
   *
   * @param events    Events to process
   * @param workspace Current workspace state
   * @return Source-code with events applied
   */
  def applyEvents(events: ArraySeq[WorkspaceFileEvent],
                  workspace: WorkspaceState.SourceAware): ArraySeq[SourceCodeState] =
    events.foldLeft(workspace.sourceCode) {
      case (newSourceCode, event) =>
        // process files & folders that belong to the configured contractPath
        if (URIUtil.contains(workspace.build.contractURI, event.uri))
          event match {
            case WorkspaceFileEvent.Deleted(uri) =>
              newSourceCode filter {
                state =>
                  // Delete files that are within the deleted uri
                  !URIUtil.contains(uri, state.fileURI)
              }

            case WorkspaceFileEvent.Created(uri) =>
              // Add or replace created source files to workspace
              if (URIUtil.getFileExtension(uri) == CompilerAccess.RALPH_FILE_EXTENSION)
                newSourceCode putIfEmpty SourceCodeState.OnDisk(uri)
              else
                newSourceCode // ignore - not a Ralph source file
          }
        else
          newSourceCode // ignore - not a Ralph source event
    }

  /**
   * Process source or build file change.
   *
   * @param fileURI File that changed.
   * @param code    Content of the file.
   */
  def changed(fileURI: URI,
              code: Option[String],
              currentWorkspace: WorkspaceState.SourceAware)(implicit file: FileAccess,
                                                            compiler: CompilerAccess): Either[ErrorUnknownFileType, WorkspaceChangeResult] = {
    val fileExtension =
      URIUtil.getFileExtension(fileURI)

    if (fileExtension == Build.BUILD_FILE_EXTENSION) {
      // process build change
      val result =
        Workspace.buildChanged(
          buildURI = fileURI,
          code = code,
          workspace = currentWorkspace
        )

      val buildChanged =
        WorkspaceChangeResult.BuildChanged(result)

      Right(buildChanged)
    } else if (fileExtension == CompilerAccess.RALPH_FILE_EXTENSION) {
      // process source code change
      val sourceResult =
        Workspace.sourceCodeChanged(
          fileURI = fileURI,
          updatedCode = code,
          workspace = currentWorkspace
        )

      Right(WorkspaceChangeResult.SourceChanged(sourceResult))
    } else {
      Left(ErrorUnknownFileType(fileURI))
    }
  }

  /**
   * Process changes to a valid build file.
   *
   * If the build file is valid, this drops existing compilations
   * and starts a fresh workspace.
   *
   * @param buildURI Location of the build file.
   * @param code     Build file's content.
   */
  def buildChanged(buildURI: URI,
                   code: Option[String],
                   workspace: WorkspaceState.SourceAware)(implicit file: FileAccess,
                                                          compiler: CompilerAccess): Option[Either[BuildState.BuildErrored, WorkspaceState.SourceAware]] =
    if (workspace.buildURI.resolve(buildURI) == workspace.buildURI) // Check: Is this fileURI an updated version of the current workspace build
      Build.parseAndCompile(
        buildURI = buildURI,
        code = code,
        currentBuild = workspace.build
      ) match {
        case Some(newBuild) =>
          newBuild match {
            case build: BuildState.BuildCompiled =>
              if (workspace.build.contractURI == build.contractURI) {
                // source directory is the same, compile using existing source-code
                val newWorkspace =
                  WorkspaceState.UnCompiled(
                    build = build,
                    sourceCode = workspace.sourceCode
                  )

                val result =
                  parseAndCompile(newWorkspace)

                Some(Right(result))
              } else {
                // directory changed, re-initialise the workspace and compile.
                val result =
                  initialise(build) map parseAndCompile

                Some(result)
              }

            case errored: BuildState.BuildErrored =>
              Some(Left(errored))
          }

        case None =>
          // no build change occurred, using existing workspace.
          Some(Right(workspace))
      }
    else
      None // ignore file that is not in the root workspace directory

  /**
   * Process changes to ralph code files.
   *
   * @param fileURI     Location of the source file.
   * @param updatedCode Source changes
   */
  def sourceCodeChanged(fileURI: URI,
                        updatedCode: Option[String],
                        workspace: WorkspaceState)(implicit file: FileAccess,
                                                   compiler: CompilerAccess): Either[BuildState.BuildErrored, WorkspaceState.SourceAware] =
    build(workspace) map {
      workspace =>
        if (URIUtil.contains(workspace.build.contractURI, fileURI)) {
          // source belongs to this workspace, process compilation including this file's changed code.
          val newSourceCode =
            downgradeSourceState(
              fileURI = fileURI,
              updatedCode = updatedCode,
              sourceCode = workspace.sourceCode
            )

          // create new un-compiled workspace.
          val unCompiledWorkspace =
            WorkspaceState.UnCompiled(
              build = workspace.build,
              sourceCode = newSourceCode
            )

          // parse and compile the new state.
          Workspace.parseAndCompile(unCompiledWorkspace)
        } else {
          // file does not belong to this workspace, do not compile it and return initialised workspace.
          workspace
        }
    }

  /**
   * Downgrade the current state of updated source-code so it gets re-parsed and re-compiled.
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
        sourceCode put newState

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

            sourceCode put newState

          case Right(exists) =>
            if (exists) {
              // source-code exists, set it as on-disk so it gets read during the next parse & compilation.
              val newState =
                SourceCodeState.OnDisk(fileURI)

              sourceCode put newState
            } else {
              // file does not exist, remove it.
              sourceCode.filter(_.fileURI != fileURI)
            }
        }
    }

}
