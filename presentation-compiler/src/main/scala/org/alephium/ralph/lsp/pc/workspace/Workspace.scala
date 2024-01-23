package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.util.CollectionUtil._
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, BuildValidator}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState.BuildCompiled
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorUnknownFileType

import java.net.URI
import scala.collection.immutable.ArraySeq

/**
 * Implements functions that operate on all source-code files within a workspace.
 *
 * All functions are immutable, returning the next workspace state given the current state.
 *
 * The phases of compiler execution:
 *  - [[Workspace.create]] - Creates a new workspace.
 *  - [[Workspace.build]] - Builds the created workspace. A valid workspace must contain a build file, i.e., `ralph.json`. The `build` function also invokes `initialise` on success to return a workspace state.
 *  - [[Workspace.initialise]] - Invoked by the build phase to create an initialised workspace with a valid build file.
 *  - [[Workspace.parse]] - Parses an initialised workspace.
 *  - [[Workspace.compile]] - Compiles a parsed workspace.
 */

object Workspace extends StrictImplicitLogging {

  /** First stage of a workspace where just the root workspace folder is known */
  def create(workspaceURI: URI): WorkspaceState.Created =
    WorkspaceState.Created(workspaceURI)

  /**
   * Returns existing workspace or initialises a new one from the configured build file.
   * Or else reports all workspace issues.
   *
   * @note Does not update the current state. The caller should set the new state.
   */
  def build(workspace: WorkspaceState)(implicit file: FileAccess,
                                       compiler: CompilerAccess,
                                       logger: ClientLogger): Either[BuildState.BuildErrored, WorkspaceState.IsSourceAware] =
    workspace match {
      case workspace: WorkspaceState.IsSourceAware =>
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
            workspace: WorkspaceState.Created)(implicit file: FileAccess,
                                               compiler: CompilerAccess,
                                               logger: ClientLogger): Either[BuildState.BuildErrored, WorkspaceState.UnCompiled] =
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
            dependency = None,
            activateWorkspace = None
          )

        Left(buildError)

      case Right(buildURI) =>
        val build =
          Build.parseAndCompile(
            buildURI = buildURI,
            code = code,
            currentBuild = None,
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
            workspace: WorkspaceState)(implicit file: FileAccess,
                                       compiler: CompilerAccess,
                                       logger: ClientLogger): Either[BuildState.BuildErrored, WorkspaceState.IsSourceAware] =
    workspace match {
      case sourceAware: WorkspaceState.IsSourceAware =>
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

  def build(buildCode: Option[String],
            currentBuild: BuildState.BuildCompiled,
            sourceCode: ArraySeq[SourceCodeState])(implicit file: FileAccess,
                                                   compiler: CompilerAccess,
                                                   logger: ClientLogger): Option[Either[BuildState.BuildErrored, WorkspaceState.UnCompiled]] =
    Build.parseAndCompile(
      buildURI = currentBuild.buildURI,
      code = buildCode,
      currentBuild = currentBuild
    ) map {
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
                dependency = currentBuild.dependency,
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

  /** Creates an un-compiled workspace for a successful build file. */
  def initialise(state: BuildState.IsCompiled)(implicit file: FileAccess): Either[BuildState.BuildErrored, WorkspaceState.UnCompiled] =
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
            dependency = state.dependency,
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
   * Parse source-code in that is not already in parsed state.
   *
   * @return A new workspace state with errors if parse fails
   *         or [[WorkspaceState.Parsed]] is returned on successful parse.
   */
  def parse(workspace: WorkspaceState.UnCompiled)(implicit file: FileAccess,
                                                  compiler: CompilerAccess): WorkspaceState.IsSourceAware =
    if (workspace.sourceCode.isEmpty) {
      workspace
    } else {
      // Parse all source code. TODO: Could be concurrent.
      val triedParsedStates =
        workspace.sourceCode map SourceCode.parse

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
                                                            compiler: CompilerAccess): WorkspaceState.IsSourceAware =
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
  def compile(workspace: WorkspaceState.Parsed)(implicit compiler: CompilerAccess): WorkspaceState.IsCompiled = {
    val compilationResult =
      SourceCode.compile(
        sourceCode = workspace.sourceCode,
        dependency = workspace.build.dependency.map(_.sourceCode),
        compilerOptions = workspace.build.config.compilerOptions
      )

    WorkspaceStateBuilder.toWorkspaceState(
      currentState = workspace,
      compilationResult = compilationResult
    )
  }

  /**
   * Compile the source-code with the new build-code.
   *
   * @param buildCode    New build-code
   * @param sourceCode   Source-code to compile
   * @param currentBuild Current build
   * @return This function will always re-compile the build,
   *         so the output is always [[WorkspaceChangeResult.BuildChanged]].
   */
  def compile(buildCode: Option[String],
              sourceCode: ArraySeq[SourceCodeState],
              currentBuild: BuildState.BuildCompiled)(implicit file: FileAccess,
                                                      compiler: CompilerAccess,
                                                      logger: ClientLogger): WorkspaceChangeResult.BuildChanged = {
    // re-build the build file
    val buildResult =
      build(
        buildCode = buildCode,
        currentBuild = currentBuild,
        sourceCode = sourceCode
      )

    val cleanBuildResult =
      buildResult match {
        case Some(buildResult) =>
          buildResult

        case None =>
          // No build change occurred. Process the new source-code with exiting build.
          val newWorkspace =
            WorkspaceState.UnCompiled(
              build = currentBuild,
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

  /** Delete or create a file within the workspace that may or may not be source-aware ([[WorkspaceState.IsSourceAware]]) */
  def deleteOrCreate(events: ArraySeq[WorkspaceFileEvent],
                     buildErrors: Option[BuildState.BuildErrored],
                     workspace: WorkspaceState)(implicit file: FileAccess,
                                                compiler: CompilerAccess,
                                                logger: ClientLogger): WorkspaceChangeResult.BuildChanged =
    workspace match {
      case aware: WorkspaceState.IsSourceAware =>
        // already source-aware, execute it!
        deleteOrCreate(
          events = events,
          buildErrors = buildErrors,
          workspace = aware
        )

      case created: WorkspaceState.Created =>
        // not source-aware, build and then execute.
        Workspace.build(
          code = None,
          workspace = created
        ) match {
          case Left(error) =>
            WorkspaceChangeResult.BuildChanged(Some(Left(error)))

          case Right(aware) =>
            deleteOrCreate(
              events = events,
              buildErrors = buildErrors,
              workspace = aware
            )
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
                     workspace: WorkspaceState.IsSourceAware)(implicit file: FileAccess,
                                                              compiler: CompilerAccess,
                                                              logger: ClientLogger): WorkspaceChangeResult.BuildChanged = {
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

    Workspace.compile(
      buildCode = buildCode,
      sourceCode = newSourceCode,
      currentBuild = workspace.build
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
                  workspace: WorkspaceState.IsSourceAware): ArraySeq[SourceCodeState] =
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

  /** Process source or build file change for a workspace that may or may not be source-aware ([[WorkspaceState.IsSourceAware]]) */
  def changed(fileURI: URI,
              code: Option[String],
              currentWorkspace: WorkspaceState)(implicit file: FileAccess,
                                                compiler: CompilerAccess,
                                                logger: ClientLogger): Either[ErrorUnknownFileType, WorkspaceChangeResult] =
    currentWorkspace match {
      case aware: WorkspaceState.IsSourceAware =>
        // already source-aware, execute change request
        changed(
          fileURI = fileURI,
          code = code,
          currentWorkspace = aware
        )

      case created: WorkspaceState.Created =>
        // not source-aware, build and then execute change request
        Workspace.build(
          code = Some(WorkspaceFile(fileURI, code)),
          workspace = created
        ) match {
          case Left(error) =>
            Right(WorkspaceChangeResult.BuildChanged(Some(Left(error))))

          case Right(aware) =>
            changed(
              fileURI = fileURI,
              code = code,
              currentWorkspace = aware
            )
        }
    }

  /**
   * Process source or build file change.
   *
   * @param fileURI File that changed.
   * @param code    Content of the file.
   */
  def changed(fileURI: URI,
              code: Option[String],
              currentWorkspace: WorkspaceState.IsSourceAware)(implicit file: FileAccess,
                                                              compiler: CompilerAccess,
                                                              logger: ClientLogger): Either[ErrorUnknownFileType, WorkspaceChangeResult] = {
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
                   workspace: WorkspaceState.IsSourceAware)(implicit file: FileAccess,
                                                            compiler: CompilerAccess,
                                                            logger: ClientLogger): Option[Either[BuildState.BuildErrored, WorkspaceState.IsSourceAware]] =
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

        case None => // no build change occurred
          val newWorkspace =
            workspace match {
              case unCompiled: WorkspaceState.UnCompiled =>
                // Build did not change but the workspace is still in un-compiled state. Parse and compile the workspace.
                logger.debug("Build did not change. Compiling un-compiled workspace.")
                parseAndCompile(unCompiled)

              case workspace =>
                // Already parsed and/or compiled. Continue with existing workspace.
                workspace
            }

          Some(Right(newWorkspace))
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
                                                   compiler: CompilerAccess,
                                                   logger: ClientLogger): Either[BuildState.BuildErrored, WorkspaceState.IsSourceAware] =
    build(workspace) map {
      workspace =>
        if (URIUtil.contains(workspace.build.contractURI, fileURI)) {
          // source belongs to this workspace, process compilation including this file's changed code.
          val newSourceCode =
            SourceCode.putOrRemove(
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
          // file does not belong to this workspace, do not compile it and return the current workspace.
          workspace
        }
    }

}
