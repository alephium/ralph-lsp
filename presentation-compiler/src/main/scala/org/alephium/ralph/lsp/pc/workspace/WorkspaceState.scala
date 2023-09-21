package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import java.net.URI
import scala.collection.immutable.ArraySeq

sealed trait WorkspaceState {
  def workspaceURI: URI
}

object WorkspaceState {

  /**
   * Workspace state with successfully configured build file.
   *
   * Parsing and compilation is implemented only for these types.
   * Until then, the workspace remains in [[Initialised]] or [[BuildCompiled]] state where
   * the user is reported any validation errors in the build file
   * for that workspace.
   * */
  sealed trait BuildAware extends WorkspaceState {
    def build: WorkspaceBuild

    def workspaceURI: URI =
      build.workspaceURI
  }

  /** Workspace state where the source-code is known and can be parsed and compiled */
  sealed trait SourceAware extends BuildAware {
    /** A workspace contains multiple source files */
    def sourceCode: ArraySeq[SourceCodeState]
  }

  /** State: Represents a compilation run result */
  sealed trait CompilerRun extends WorkspaceState.SourceAware {
    def parsed: WorkspaceState.Parsed
  }

  /** State: IDE is initialised but the build file requires validation */
  case class Initialised(workspaceURI: URI) extends WorkspaceState

  /** State: Build file is compiled. This state can upgraded to [[UnCompiled]]. */
  case class BuildCompiled(build: WorkspaceBuild) extends BuildAware {
    override def workspaceURI: URI =
      build.workspaceURI
  }

  /** State: Source files might be un-compiled, parsed or compiled. This state can be parsed and compiled. */
  case class UnCompiled(build: WorkspaceBuild,
                        sourceCode: ArraySeq[SourceCodeState]) extends WorkspaceState.SourceAware

  /** State: All source files parsed, therefore this workspace can be compiled */
  case class Parsed(build: WorkspaceBuild,
                    sourceCode: ArraySeq[SourceCodeState.Parsed]) extends WorkspaceState.SourceAware

  /**
   * Result of an errored compiler run.
   *
   * @param sourceCode      New valid source code states.
   * @param workspaceErrors Project/workspace level errors
   * @param parsed          Previous valid parsed state (used for code completion in-case the file has error)
   */
  case class Errored(sourceCode: ArraySeq[SourceCodeState],
                     workspaceErrors: ArraySeq[FormattableError],
                     parsed: WorkspaceState.Parsed) extends CompilerRun {
    def build: WorkspaceBuild =
      parsed.build
  }

  /**
   * Result of a successful compiler run.
   *
   * @param sourceCode New valid source code states.
   * @param parsed     Current parser run for this compiled code.
   */
  case class Compiled(sourceCode: ArraySeq[SourceCodeState],
                      parsed: WorkspaceState.Parsed) extends CompilerRun {
    def build: WorkspaceBuild =
      parsed.build
  }

}
