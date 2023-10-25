package org.alephium.ralph.lsp.server.converter

import fastparse.IndexedParserInput
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.SourcePosition
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.alephium.ralph.lsp.server.state.ServerState
import org.eclipse.lsp4j._

import java.net.URI
import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.SeqHasAsJava

/** Functions that transform internal diagnostics types to LSP4J types */
object DiagnosticsConverter {

  /**
   * Given the current [[ServerState]] and the next [[ServerState]],
   * return new diagnostics to publish.
   *
   * @return Diagnostics clearing resolved diagnostics dispatched in previous state.
   */
  def toPublishDiagnostics(currentState: ServerState,
                           newState: ServerState): Iterable[PublishDiagnosticsParams] = {
    // fetch diagnostics to publish for the build file
    val buildDiagnostics =
      toPublishDiagnostics(
        buildURI = currentState.workspace.map(_.buildURI),
        currentBuildErrors = currentState.buildErrors,
        newBuildErrors = newState.buildErrors
      )

    // fetch diagnostics to publish for the source-code
    val workspaceDiagnostics =
      toPublishDiagnostics(
        currentWorkspace = currentState.workspace,
        newWorkspace = newState.workspace
      )

    buildDiagnostics ++ workspaceDiagnostics
  }

  /**
   * Given the current build-errors and the next,
   * return diagnostics to publish to the client.
   * */
  def toPublishDiagnostics(buildURI: Option[URI],
                           currentBuildErrors: Option[BuildState.BuildErrored],
                           newBuildErrors: Option[BuildState.BuildErrored]): Option[PublishDiagnosticsParams] =
    (currentBuildErrors, newBuildErrors) match {
      case (Some(build), None) =>
        // build errors were fixed. Clear old errors
        val diagnostics =
          toPublishDiagnostics(
            fileURI = build.buildURI,
            code = build.code,
            errors = List.empty,
            severity = DiagnosticSeverity.Error
          )

        Some(diagnostics)

      case (_, Some(build)) =>
        // New state has errors, create diagnostics.
        val diagnostics =
          toPublishDiagnostics(
            fileURI = build.buildURI,
            code = build.code,
            errors = build.errors.to(List),
            severity = DiagnosticSeverity.Error
          )

        Some(diagnostics)

      case (None, None) =>
        None
    }

  /**
   * Given the current workspace and the next,
   * return diagnostics to publish to the client.
   * */
  def toPublishDiagnostics(currentWorkspace: Option[WorkspaceState],
                           newWorkspace: Option[WorkspaceState]): Iterable[PublishDiagnosticsParams] =
    (currentWorkspace, newWorkspace) match {
      case (Some(current), None) =>
        toPublishDiagnostics(current)

      case (None, Some(next)) =>
        toPublishDiagnostics(next)

      case (Some(current), Some(next)) =>
        toPublishDiagnostics(
          currentWorkspace = current,
          newWorkspace = next
        )

      case (None, None) =>
        None
    }

  /** Fetch all diagnostics for this workspace */
  def toPublishDiagnostics(currentWorkspace: WorkspaceState): Iterable[PublishDiagnosticsParams] =
    currentWorkspace match {
      case _: WorkspaceState.Created =>
        Iterable.empty

      case currentWorkspace: WorkspaceState.SourceAware =>
        // publish new workspace given previous workspace.
        toPublishDiagnostics(
          previousOrCurrentState = currentWorkspace,
          nextState = None
        )
    }

  /**
   * Given the current workspace and the next, fetch diagnostics to dispatch, clearing resolved diagnostics.
   * */
  def toPublishDiagnostics(currentWorkspace: WorkspaceState,
                           newWorkspace: WorkspaceState): Iterable[PublishDiagnosticsParams] =
    (currentWorkspace, newWorkspace) match {
      case (_: WorkspaceState.Created, newWorkspace: WorkspaceState.SourceAware) =>
        // publish first compilation result i.e. previous workspace had no compilation run.
        toPublishDiagnostics(
          previousOrCurrentState = newWorkspace,
          nextState = None
        )

      case (currentWorkspace: WorkspaceState.SourceAware, newWorkspace: WorkspaceState) =>
        // publish new workspace given previous workspace.
        toPublishDiagnostics(
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
  def toPublishDiagnostics(previousOrCurrentState: WorkspaceState.SourceAware,
                           nextState: Option[WorkspaceState]): Iterable[PublishDiagnosticsParams] = {
    // build diagnostics sent for previous state, or the current state if this is the first run.
    val previousOrCurrentDiagnotics =
      toPublishDiagnostics(previousOrCurrentState)

    nextState match {
      case Some(nextState) =>
        // diagnostics to send for the current run
        val newDiagnostics =
          toPublishDiagnostics(nextState)

        // Collects diagnostics published in previous run that are now resolved.
        val resolvedDiagnostics =
          ListBuffer.empty[PublishDiagnosticsParams]

        // build diagnostics that are resolved
        previousOrCurrentDiagnotics foreach {
          previous =>
            if (!newDiagnostics.exists(_.getUri == previous.getUri)) {
              // build a diagnostic to clear old published diagnostics
              val resolved = new PublishDiagnosticsParams(previous.getUri, util.Arrays.asList())
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

  /** Convert Ralph's FormattableError to lsp4j's Diagnostic */
  def toDiagnostic(code: Option[String],
                   message: CompilerMessage,
                   severity: DiagnosticSeverity): Diagnostic = {
    val range =
      code match {
        case Some(code) =>
          val fastParseLineNumber = IndexedParserInput(code).prettyIndex(message.index.index)
          val sourcePosition = SourcePosition.parse(fastParseLineNumber)

          val start = new Position(sourcePosition.rowIndex, sourcePosition.colIndex)
          val end = new Position(sourcePosition.rowIndex, sourcePosition.colIndex + message.index.width)
          new Range(start, end)

        case None =>
          // If source-code text is not known, then the line-number can't be fetched.
          // So return this error at file-level with an empty range.
          new Range(new Position(0, 0), new Position(0, 0))
      }

    new Diagnostic(range, message.message, severity, "Ralph")
  }

  /** Fetch workspace/project level diagnostics i.e. diagnostics that do have source information. */
  def toWorkspaceDiagnostics(workspace: WorkspaceState.SourceAware): PublishDiagnosticsParams = {
    val workspaceDiagnostics =
      workspace match {
        case compiled: WorkspaceState.Errored =>
          compiled.workspaceErrors map {
            error =>
              // These are workspace level errors such as `Compiler.Error`, their source-code information is unknown.
              toDiagnostic(
                code = None,
                message = error,
                severity = DiagnosticSeverity.Error
              )
          }

        case _ =>
          Seq.empty
      }

    new PublishDiagnosticsParams(workspace.workspaceURI.toString, workspaceDiagnostics.asJava)
  }

  /** Fetch source-code level diagnostics */
  def toSourceCodeDiagnostics(state: WorkspaceState.SourceAware): Iterable[PublishDiagnosticsParams] =
    state.sourceCode collect {
      case state: SourceCodeState.ErrorSource =>
        // transform multiple source code errors to diagnostics.
        val diagnostics =
          state.errors map {
            error =>
              toDiagnostic(
                code = Some(state.code),
                message = error,
                severity = DiagnosticSeverity.Error
              )
          }

        new PublishDiagnosticsParams(state.fileURI.toString, diagnostics.asJava)

      case state: SourceCodeState.ErrorAccess =>
        // transform single source code access error to diagnostics.
        val diagnostics =
          toDiagnostic(
            code = None,
            message = state.error,
            severity = DiagnosticSeverity.Error
          )

        new PublishDiagnosticsParams(state.fileURI.toString, util.Arrays.asList(diagnostics))

      case state: SourceCodeState.Compiled =>
        // transform source code warning messages to diagnostics.
        val diagnostics =
          state.warnings map {
            warning =>
              toDiagnostic(
                code = Some(state.code),
                message = warning,
                severity = DiagnosticSeverity.Warning
              )
          }

        new PublishDiagnosticsParams(state.fileURI.toString, diagnostics.asJava)
    }

  /** Fetch all diagnostics for this workspace */
  def toPublishDiagnostics(workspace: WorkspaceState.SourceAware): Iterable[PublishDiagnosticsParams] = {
    val sourceCodeDiagnostics = toSourceCodeDiagnostics(workspace)
    val workspaceDiagnostics = toWorkspaceDiagnostics(workspace)
    sourceCodeDiagnostics ++ Seq(workspaceDiagnostics)
  }

  /** Build a diagnostic instance give the error and file information */
  def toPublishDiagnostics(fileURI: URI,
                           code: Option[String],
                           errors: List[CompilerMessage.AnyError],
                           severity: DiagnosticSeverity): PublishDiagnosticsParams = {
    val diagnostics =
      errors map {
        error =>
          toDiagnostic(
            code = code,
            message = error,
            severity = severity
          )
      }

    new PublishDiagnosticsParams(fileURI.toString, diagnostics.asJava)
  }
}
