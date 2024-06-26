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
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}

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
   * Build only a [[WorkspaceState.Created]] workspace.
   * All other workspace states are returned as-is, they do not get re-compiled.
   *
   * @param workspace The workspace to build.
   * @return A built workspace which is source and build aware.
   */
  def build(
      workspace: WorkspaceState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[BuildState.Errored, WorkspaceState.IsSourceAware] =
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

  /**
   * If already built, return existing workspace or else invoke build.
   *
   * @param code File that changed.
   * @return Returns diagnostics in-case were build errors, or-else returns an initialised workspace.
   */
  def build(
      code: Option[WorkspaceFile],
      workspace: WorkspaceState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[BuildState.Errored, WorkspaceState.IsSourceAware] =
    workspace match {
      case sourceAware: WorkspaceState.IsSourceAware =>
        // already built
        Right(sourceAware)

      case currentWorkspace: WorkspaceState.Created =>
        // workspace is created but it's not built yet. Let's build it!
        code match {
          case Some(code) if code.fileURI == currentWorkspace.buildURI =>
            // this request is for the build file, build it using the code sent by the client
            val build =
              Build.parseAndCompile(
                buildURI = code.fileURI,
                code = code.text,
                currentBuild = None
              )

            initialise(build)

          case _ =>
            // else build from disk
            Workspace.build(currentWorkspace)
        }
    }

  /**
   * Build without any [[WorkspaceState]] information.
   *
   * Source-code is built just from the input build information
   * and is synchronised with source files on disk.
   *
   * @param newBuildCode New build file's text content.
   * @param currentBuild Currently known compiled build.
   * @param sourceCode   The source-code to initialise into a un-compiled workspace.
   * @return An un-compiled workspace with the new compiled build file.
   */
  def build(
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
        currentBuild = currentBuild
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
      compiler: CompilerAccess): WorkspaceState.IsParsedAndCompiled =
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
  def compile(workspace: WorkspaceState.Parsed)(implicit compiler: CompilerAccess): WorkspaceState.IsCompiled = {
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
    build(
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
   * Process changes to a valid build file.
   *
   * If the build file is valid, this drops existing compilations
   * and starts a fresh workspace.
   *
   * @param buildURI Location of the build file.
   * @param code     Build file's content.
   */
  def buildChanged(
      buildURI: URI,
      code: Option[String],
      workspace: WorkspaceState.IsSourceAware
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Option[Either[BuildState.Errored, WorkspaceState.IsSourceAware]] =
    if (workspace.buildURI.resolve(buildURI) == workspace.buildURI) // Check: Is this fileURI an updated version of the current workspace build
      Build.parseAndCompile(
        buildURI = buildURI,
        code = code,
        currentBuild = workspace.build
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
                  initialise(build) map parseAndCompile

                Some(result)
              }

            case errored: BuildState.Errored =>
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
  def sourceCodeChanged(
      fileURI: URI,
      updatedCode: Option[String],
      workspace: WorkspaceState
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Either[BuildState.Errored, WorkspaceState.IsSourceAware] =
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
