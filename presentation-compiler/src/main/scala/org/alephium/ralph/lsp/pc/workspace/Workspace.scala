// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.util.CollectionUtil._
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.pc.workspace.build.typescript.TSBuild
import org.alephium.ralph.lsp.pc.workspace.build.{BuildError, Build, BuildState}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.DependencyDownloader

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
private[pc] object Workspace extends StrictImplicitLogging {

  /** First stage of a workspace where just the root workspace folder is known */
  def create(workspaceURI: URI): WorkspaceState.Created =
    WorkspaceState.Created(workspaceURI)

  /**
   * Incrementally builds the workspace.
   *
   * If the input workspace is already built, it returns the existing workspace state.
   *
   * @param code      The file that changed.
   * @param workspace The current state of the workspace.
   * @return Either build errors or an initialised workspace state that is source-aware.
   */
  def build(
      code: Option[WorkspaceFile],
      workspace: WorkspaceState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[BuildError, WorkspaceState.IsSourceAware] =
    workspace match {
      case workspace: WorkspaceState.IsSourceAware =>
        // Workspace is already built and is source-aware. Return it!
        Right(workspace)

      case workspace: WorkspaceState.Created =>
        // workspace is created but it's not built yet. Let's build it!
        buildClean(
          code = code,
          workspace = workspace
        )
    }

  /**
   * Executes build on a newly created workspace.
   *
   * @param code      The file that changed.
   * @param workspace The created workspace.
   * @return Either build errors or an [[WorkspaceState.UnCompiled]] workspace state.
   */
  def buildClean(
      code: Option[WorkspaceFile],
      workspace: WorkspaceState.Created
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[BuildError, WorkspaceState.UnCompiled] =
    code match {
      case Some(code) if code.fileURI == workspace.tsBuildURI =>
        // Request is for the `alephium.config.ts` build file.
        // Built the workspace!
        val build =
          Build.parseAndCompile(
            buildURI = workspace.buildURI,
            code = None,
            currentBuild = None,
            dependencyDownloaders = DependencyDownloader.natives()
          )

        // Build `alephium.config.ts` using `ralph.json`'s compilation result.
        TSBuild.build(
          code = code.text,
          currentBuild = build
        ) match {
          case Left(error) =>
            // TSBuild errored. This error state contains all errors.
            Left(BuildError(error))

          case Right(None) =>
            // No change occurred to `ralph.json`, initialise workspace using the build above.
            initialise(build)
              .left
              .map(BuildError(_))

          case Right(Some(parsed: BuildState.Parsed)) =>
            // An updated `ralph.json` was persisted, do a clean build on the created workspace.
            buildClean(
              // the newly persisted `ralph.json` file content is known, provide WorkspaceFile so no disk read occurs.
              code = Some(WorkspaceFile(parsed.buildURI, Some(parsed.code))),
              workspace = workspace
            )
        }

      case Some(code) if code.fileURI == workspace.buildURI =>
        // this request is for the `ralph.json` build file, build it using the code sent by the client (no disk IO).
        val build =
          Build.parseAndCompile(
            buildURI = code.fileURI,
            code = code.text,
            currentBuild = None,
            dependencyDownloaders = DependencyDownloader.natives()
          )

        initialise(build)
          .left
          .map(BuildError(_))

      case Some(_) | None =>
        // this code file is not a build file, so build from disk.
        val build =
          Build.parseAndCompile(
            buildURI = workspace.buildURI,
            code = None,
            currentBuild = None,
            dependencyDownloaders = DependencyDownloader.natives()
          )

        initialise(build)
          .left
          .map(BuildError(_))
    }

  /**
   * Builds a workspace without any existing [[WorkspaceState]] information.
   *
   * Source-code is built just from the input build information
   * and is synchronised with source files on disk,
   * ensuring all source files on-disk are known to this build.
   *
   * @param newBuildCode New build file's text content.
   * @param currentBuild Currently known compiled build.
   * @param sourceCode   The source-code to initialise into a un-compiled workspace.
   * @return An un-compiled workspace with the new compiled build file.
   */
  def buildSynchronised(
      newBuildCode: Option[String],
      currentBuild: BuildState.Compiled,
      sourceCode: ArraySeq[SourceCodeState]
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[BuildState.Errored, WorkspaceState.UnCompiled] = {
    val newBuild =
      Build.parseAndCompile(
        buildURI = currentBuild.buildURI,
        code = newBuildCode,
        currentBuild = currentBuild,
        dependencyDownloaders = DependencyDownloader.natives()
      ) getOrElse currentBuild // if the build code is the same and existing build, then compile using existing build.

    newBuild match {
      case newBuild: BuildState.Compiled =>
        // Build compiled OK! Compile new source-files with this new build file
        val syncedCode =
          SourceCode.synchronise(
            sourceDirectory = newBuild.contractURI,
            sourceCode = sourceCode
          )

        syncedCode match {
          case Left(error) =>
            // activate this workspace so that the most recently source-code is available.
            val activateWorkspace =
              WorkspaceState.UnCompiled(
                build = newBuild,
                sourceCode = sourceCode
              )

            val buildErrored =
              BuildState.Errored(
                buildURI = newBuild.buildURI,
                codeOption = Some(newBuild.code),
                errors = ArraySeq(error),
                dependencies = currentBuild.dependencies,
                activateWorkspace = Some(activateWorkspace)
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

      case errored: BuildState.Errored =>
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
  }

  /** Creates an un-compiled workspace for a successful build file. */
  def initialise(state: BuildState.IsCompiled)(implicit file: FileAccess): Either[BuildState.Errored, WorkspaceState.UnCompiled] =
    state match {
      case compiled: BuildState.Compiled =>
        // Build file changed. Update the workspace and request a full workspace build.
        initialise(compiled)

      case errored: BuildState.Errored =>
        Left(errored)
    }

  /**
   * Initial workspaces collects paths to all OnDisk ralph files.
   *
   * @param state Current state of the workspace.
   * @return New workspace state which aware of all workspace source code files.
   */
  def initialise(state: BuildState.Compiled)(implicit file: FileAccess): Either[BuildState.Errored, WorkspaceState.UnCompiled] =
    SourceCode.initialise(state.contractURI) match {
      case Left(error) =>
        val buildError =
          BuildState.Errored(
            buildURI = state.buildURI,
            codeOption = Some(state.code),
            errors = ArraySeq(error),
            dependencies = state.dependencies,
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
  def parse(
      workspace: WorkspaceState.UnCompiled
    )(implicit file: FileAccess,
      compiler: CompilerAccess): WorkspaceState.IsParsed = {
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
  def parseAndCompile(
      workspace: WorkspaceState.UnCompiled
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): WorkspaceState.IsParsedAndCompiled =
    parse(workspace) match {
      case unCompiled: WorkspaceState.UnCompiled =>
        // Still un-compiled. There are errors.
        unCompiled

      case parsed: WorkspaceState.Parsed =>
        // Successfully parsed! Compile it!
        compile(parsed)
    }

  /**
   * Compile a parsed workspace.
   */
  def compile(
      workspace: WorkspaceState.Parsed
    )(implicit compiler: CompilerAccess,
      logger: ClientLogger): WorkspaceState.IsCompiled = {
    val compilationResult =
      SourceCode.compile(
        sourceCode = workspace.sourceCode,
        dependency = workspace.build.findDependency(DependencyID.Std).to(ArraySeq).flatMap(_.sourceCode),
        compilerOptions = workspace.build.config.compilerOptions,
        workspaceErrorURI = workspace.workspaceURI
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
   * @return New compiled workspace state.
   */
  def compile(
      buildCode: Option[String],
      sourceCode: ArraySeq[SourceCodeState],
      currentBuild: BuildState.Compiled
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[BuildState.Errored, WorkspaceState.IsParsedAndCompiled] =
    // re-build the build file
    buildSynchronised(
      newBuildCode = buildCode,
      currentBuild = currentBuild,
      sourceCode = sourceCode
    ) map parseAndCompile

  /**
   * Apply events to workspace source-code.
   *
   * @param events    Events to process
   * @param workspace Current workspace state
   * @return Source-code with events applied
   */
  def applyEvents(
      events: ArraySeq[WorkspaceFileEvent],
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
              // Add or replace created source files
              if (URIUtil.isRalphFileExtension(uri))
                newSourceCode putIfEmpty SourceCodeState.OnDisk(uri)
              else
                newSourceCode // ignore - not a Ralph source file

            case WorkspaceFileEvent.Changed(uri) =>
              // Replace changed source files
              if (URIUtil.isRalphFileExtension(uri))
                newSourceCode put SourceCodeState.OnDisk(uri)
              else
                newSourceCode // ignore - not a Ralph source file
          }
        else
          newSourceCode // ignore - not a Ralph source event
    }

  /**
   * Processes changes to a valid build file (`ralph.json` or `alephium.config.ts`).
   *
   * If the build file is valid and a change has occurred, this function drops existing compilation
   * and initialises a new workspace, and re-builds it.
   *
   * @param buildURI    Location of the build file.
   * @param code        Content of the build file, if available, otherwise it's read from the disk, if required.
   * @param workspace   The current workspace with a successfully compiled JSON build file (`ralph.json`).
   * @param buildErrors Provided when there are errors in the latest JSON build file (`ralph.json`).
   * @return None if no change has occurred, otherwise, either a build error or an updated workspace state.
   */
  def buildChanged(
      buildURI: URI,
      code: Option[String],
      workspace: WorkspaceState.IsSourceAware,
      buildErrors: Option[BuildState.Errored]
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Option[Either[BuildError, WorkspaceState.IsSourceAware]] =
    if (workspace.workspaceURI.resolve(buildURI) == workspace.tsBuildURI) // Request is for the `alephium.config.ts` build file.
      TSBuild.build(
        code = code,
        // Prefer a build with errors over a successfully compiled workspace build to ensure the most recent build is processed.
        currentBuild = buildErrors getOrElse workspace.build
      ) match {
        case Left(error) =>
          // TypeScript build reported error.
          Some(Left(BuildError(error)))

        case Right(Some(parsed: BuildState.Parsed)) =>
          // `alephium.config.ts` was built successfully, with an updated `ralph.json` persisted.
          // Trigger a re-build of `ralph.json` and re-compilation of the workspace.
          buildChanged(
            buildURI = parsed.buildURI,
            code = Some(parsed.code), // the newly persisted `ralph.json` file content is known, provide the code so no disk read occurs.
            workspace = workspace,
            buildErrors = buildErrors
          )

        case Right(None) =>
          // `alephium.config.ts` build successful, but no change occurred to `ralph.json`,
          // so existing build remains the same, no need to re-compile.
          None
      }
    else if (workspace.buildURI.resolve(buildURI) == workspace.buildURI) // Check: Is this fileURI an updated version of the current workspace build
      Build.parseAndCompile(
        buildURI = buildURI,
        code = code,
        currentBuild = workspace.build,
        dependencyDownloaders = DependencyDownloader.natives()
      ) match {
        case Some(newBuild) =>
          newBuild match {
            case build: BuildState.Compiled =>
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
                  initialise(build)
                    .map(parseAndCompile)
                    .left
                    .map(BuildError(_))

                Some(result)
              }

            case errored: BuildState.Errored =>
              Some(Left(BuildError(errored)))
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
  def sourceCodeChanged(
      fileURI: URI,
      updatedCode: Option[String],
      workspace: WorkspaceState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[BuildError, WorkspaceState.IsSourceAware] =
    build(
      code = Some(WorkspaceFile(fileURI, updatedCode)),
      workspace = workspace
    ) map {
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
