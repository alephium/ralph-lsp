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

package org.alephium.ralph.lsp.pc.diagnostic

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, LineRange}
import org.alephium.ralph.lsp.pc.PCState
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.pc.workspace.build.typescript.TSBuildState

import java.net.URI
import scala.collection.mutable.ListBuffer

object Diagnostics {

  /**
   * Given the current [[PCState]] and the next [[PCState]],
   * returns new diagnostics to publish.
   *
   * @param currentState The current presentation compiler state.
   * @param newState     The next presentation compiler state.
   * @return An iterator over resolved diagnostics dispatched in the previous state and new diagnostics.
   */
  def toFileDiagnostics(
      currentState: PCState,
      newState: PCState): Iterable[FileDiagnostic] = {
    // fetch diagnostics to publish for `ralph.json` build file
    val jsonBuildDiagnostics =
      toFileDiagnosticsForJSONBuild(
        currentBuildErrors = currentState.buildErrors,
        newBuildErrors = newState.buildErrors
      )

    // fetch diagnostics to publish for `alephium.config.ts` build file
    val tsBuildDiagnostics =
      toFileDiagnosticsForTSBuild(
        currentBuildErrors = currentState.tsErrors,
        newBuildErrors = newState.tsErrors
      )

    // fetch diagnostics to publish for the `*.ral` source-code files
    val workspaceDiagnostics =
      toFileDiagnosticsForWorkspace(
        currentWorkspace = Some(currentState.workspace),
        newWorkspace = Some(newState.workspace)
      )

    jsonBuildDiagnostics ++ tsBuildDiagnostics ++ workspaceDiagnostics
  }

  /**
   * Given the current build-errors and the next, return diagnostics to publish
   * for the current compilation request.
   */
  def toFileDiagnosticsForJSONBuild(
      currentBuildErrors: Option[BuildState.Errored],
      newBuildErrors: Option[BuildState.Errored]): Iterable[FileDiagnostic] =
    (currentBuildErrors, newBuildErrors) match {
      case (Some(build), None) =>
        // build errors were fixed. Clear old errors
        val buildDiagnostics =
          clearFileDiagnotics(
            fileURI = build.buildURI,
            severity = CodeDiagnosticSeverity.Error
          )

        // build diagnostics for the dependency
        val dependencyDiagnostics =
          build.dependencies.to(Array) flatMap toFileDiagnostics

        // collect all diagnostics
        dependencyDiagnostics :+ buildDiagnostics

      case (None, Some(build)) =>
        // New build has errors, create diagnostics.
        val buildDiagnostics =
          toFileDiagnostics(build)

        // build diagnostics for the dependency
        val dependencyDiagnostics =
          build.dependencies.to(Array) flatMap toFileDiagnostics

        dependencyDiagnostics :+ buildDiagnostics

      case (Some(oldBuild), Some(newBuild)) =>
        // New build has errors, build diagnostics given previous build result.
        val buildDiagnostics =
          toFileDiagnostics(newBuild)

        val dependencyDiagnostics =
          DependencyID
            .all()
            .flatMap {
              dependencyID =>
                // Build dependency diagnostics given previous dependency diagnostics.
                toFileDiagnosticsForWorkspace(
                  currentWorkspace = oldBuild.findDependency(dependencyID),
                  newWorkspace = newBuild.findDependency(dependencyID)
                )
            }

        dependencyDiagnostics :+ buildDiagnostics

      case (None, None) =>
        // No state change occurred. Nothing to diagnose.
        Iterable.empty
    }

  /**
   * Builds diagnostics for the build's error state.
   *
   * @param build The errored build.
   * @return The build file's diagnostics.
   */
  def toFileDiagnostics(build: BuildState.Errored): FileDiagnostic =
    // `ralph.json` diagnotics
    toFileDiagnostics(
      fileURI = build.buildURI,
      code = build.codeOption,
      errors = build.errors.to(List),
      severity = CodeDiagnosticSeverity.Error
    )

  /**
   * Builds diagnostics for the TypeScript `alephium.config.ts` build's error state.
   */
  def toFileDiagnosticsForTSBuild(
      currentBuildErrors: Option[TSBuildState.Errored],
      newBuildErrors: Option[TSBuildState.Errored]): Option[FileDiagnostic] =
    (currentBuildErrors, newBuildErrors) match {
      case (Some(build), None) =>
        // build errors were fixed. Clear old errors
        val cleared =
          clearFileDiagnotics(
            fileURI = build.buildURI,
            severity = CodeDiagnosticSeverity.Error
          )

        Some(cleared)

      case (None | Some(_), Some(build)) =>
        // New build has errors, create diagnostics for the new build.
        Some(toFileDiagnostics(build))

      case (None, None) =>
        // No state change occurred. Nothing to diagnose.
        None
    }

  /**
   * Builds file diagnostics for `alephium.config.ts`.
   *
   * @param state The errored state.
   * @return The build file's diagnostics.
   */
  def toFileDiagnostics(state: TSBuildState.Errored): FileDiagnostic = {
    val diagnostics =
      state.errors map {
        error =>
          toDiagnostic(
            code = state.code,
            message = error,
            severity = CodeDiagnosticSeverity.Error
          )
      }

    FileDiagnostic(state.buildURI, diagnostics)
  }

  /**
   * Given the current workspace and the next,
   * return diagnostics to publish to the client.
   */
  def toFileDiagnosticsForWorkspace(
      currentWorkspace: Option[WorkspaceState],
      newWorkspace: Option[WorkspaceState]): Iterable[FileDiagnostic] =
    (currentWorkspace, newWorkspace) match {
      case (Some(current), None) =>
        toFileDiagnostics(current)

      case (None, Some(next)) =>
        toFileDiagnostics(next)

      case (Some(current), Some(next)) =>
        toFileDiagnostics(
          currentWorkspace = current,
          newWorkspace = next
        )

      case (None, None) =>
        None
    }

  /** Fetch all diagnostics for this workspace */
  def toFileDiagnostics(currentWorkspace: WorkspaceState): Iterable[FileDiagnostic] =
    currentWorkspace match {
      case _: WorkspaceState.Created =>
        Iterable.empty

      case currentWorkspace: WorkspaceState.IsSourceAware =>
        // publish new workspace given previous workspace.
        toFileDiagnostics(
          previousOrCurrentState = currentWorkspace,
          nextState = None
        )
    }

  /**
   * Given the current workspace and the next, fetch diagnostics to dispatch, clearing resolved diagnostics.
   */
  def toFileDiagnostics(
      currentWorkspace: WorkspaceState,
      newWorkspace: WorkspaceState): Iterable[FileDiagnostic] =
    (currentWorkspace, newWorkspace) match {
      case (_: WorkspaceState.Created, newWorkspace: WorkspaceState.IsSourceAware) =>
        // publish first compilation result i.e. previous workspace had no compilation run.
        toFileDiagnostics(
          previousOrCurrentState = newWorkspace,
          nextState = None
        )

      case (currentWorkspace: WorkspaceState.IsSourceAware, newWorkspace: WorkspaceState) =>
        // publish new workspace given previous workspace.
        toFileDiagnostics(
          previousOrCurrentState = currentWorkspace,
          nextState = Some(newWorkspace)
        )

      case (_: WorkspaceState.Created, _: WorkspaceState.Created) =>
        // Nothing to publish
        Iterable.empty
    }

  /**
   * Build diagnostics to publish and clear older resolved errors or warnings.
   *
   * @param previousOrCurrentState Previous state or the current state if this is the first run.
   * @param nextState              Newest state.
   *                               Set to [[None]] if previousState is the only state.
   * @return Diagnostics to publish for the current state.
   */
  def toFileDiagnostics(
      previousOrCurrentState: WorkspaceState.IsSourceAware,
      nextState: Option[WorkspaceState]): Iterable[FileDiagnostic] = {
    // build diagnostics sent for previous state, or the current state if this is the first run.
    val previousOrCurrentDiagnotics =
      toFileDiagnostics(previousOrCurrentState)

    nextState match {
      case Some(nextState) =>
        // diagnostics to send for the current run
        val newDiagnostics =
          toFileDiagnostics(nextState)

        // Collects diagnostics published in previous run that are now resolved.
        val resolvedDiagnostics =
          ListBuffer.empty[FileDiagnostic]

        // build diagnostics that are resolved
        previousOrCurrentDiagnotics foreach {
          previous =>
            if (!newDiagnostics.exists(_.fileURI == previous.fileURI)) {
              // build a diagnostic to clear old published diagnostics
              val resolved = FileDiagnostic(previous.fileURI, Seq.empty)
              resolvedDiagnostics addOne resolved
            }
        }

        // all diagnostics to publish for this run
        resolvedDiagnostics ++ newDiagnostics

      case None =>
        // there is no next state, therefore this is the first run
        previousOrCurrentDiagnotics
    }
  }

  /** Convert Ralph's FormattableError to lsp4j's CodeDiagnostic */
  def toDiagnostic(
      code: Option[String],
      message: CompilerMessage,
      severity: CodeDiagnosticSeverity): CodeDiagnostic = {
    val range =
      code match {
        case Some(code) =>
          message.index.toLineRange(code)

        case None =>
          // If source-code text is not known, then the line-number can't be fetched.
          // So return this error at file-level with an empty range.
          LineRange.zero
      }

    CodeDiagnostic(
      range = range,
      message = message.message,
      severity = severity
    )
  }

  /** Fetch workspace/project level diagnostics i.e. diagnostics that do have source information. */
  def toWorkspaceDiagnostics(workspace: WorkspaceState.IsSourceAware): FileDiagnostic = {
    val workspaceDiagnostics =
      workspace match {
        case compiled: WorkspaceState.Errored =>
          compiled.workspaceErrors map {
            error =>
              // These are workspace level errors such as `Compiler.Error`, their source-code information is unknown.
              toDiagnostic(
                code = None,
                message = error,
                severity = CodeDiagnosticSeverity.Error
              )
          }

        case _ =>
          Seq.empty
      }

    FileDiagnostic(workspace.workspaceURI, workspaceDiagnostics)
  }

  /** Fetch source-code level diagnostics */
  def toSourceCodeDiagnostics(state: WorkspaceState.IsSourceAware): Iterable[FileDiagnostic] =
    state.sourceCode collect {
      case state: SourceCodeState.IsParserOrCompilationError =>
        // transform multiple source code errors to diagnostics.
        val diagnostics =
          state.errors map {
            error =>
              toDiagnostic(
                code = Some(state.code),
                message = error,
                severity = CodeDiagnosticSeverity.Error
              )
          }

        FileDiagnostic(state.fileURI, diagnostics)

      case state: SourceCodeState.ErrorAccess =>
        // transform single source code access error to diagnostics.
        val diagnostics =
          toDiagnostic(
            code = None,
            message = state.error,
            severity = CodeDiagnosticSeverity.Error
          )

        FileDiagnostic(state.fileURI, List(diagnostics))

      case state: SourceCodeState.Compiled =>
        // transform source code warning messages to diagnostics.
        val diagnostics =
          state.warnings map {
            warning =>
              toDiagnostic(
                code = Some(state.code),
                message = warning,
                severity = CodeDiagnosticSeverity.Warning
              )
          }

        FileDiagnostic(state.fileURI, diagnostics)
    }

  /** Fetch all diagnostics for this workspace */
  def toFileDiagnostics(workspace: WorkspaceState.IsSourceAware): Iterable[FileDiagnostic] = {
    val sourceCodeDiagnostics = toSourceCodeDiagnostics(workspace)
    val workspaceDiagnostics  = toWorkspaceDiagnostics(workspace)
    sourceCodeDiagnostics ++ Seq(workspaceDiagnostics)
  }

  /** Build a diagnostic instance give the error and file information */
  def toFileDiagnostics(
      fileURI: URI,
      code: Option[String],
      errors: List[CompilerMessage.AnyError],
      severity: CodeDiagnosticSeverity): FileDiagnostic = {
    val diagnostics =
      errors map {
        error =>
          toDiagnostic(
            code = code,
            message = error,
            severity = severity
          )
      }

    FileDiagnostic(fileURI, diagnostics)
  }

  /**
   * Clears all file diagnostics for the `fileURI`.
   *
   * @param fileURI  The file URI to clear diagnostics for.
   * @param severity The type of diagnostics to clear.
   * @return Cleared file diagnostics.
   */
  private def clearFileDiagnotics(
      fileURI: URI,
      severity: CodeDiagnosticSeverity): FileDiagnostic =
    toFileDiagnostics(
      fileURI = fileURI,
      code = None,
      errors = List.empty, // clear old errors
      severity = severity
    )

}
