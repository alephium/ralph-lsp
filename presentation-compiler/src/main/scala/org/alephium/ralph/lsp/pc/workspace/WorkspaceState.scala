package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import java.net.URI
import scala.collection.immutable.ArraySeq

sealed trait WorkspaceState {
  def workspaceURI: URI

  def buildOpt: Option[WorkspaceBuild]
}

object WorkspaceState {

  /**
   * Workspace state with successfully configured build file.
   *
   * Parsing and compilation is implemented only for these types.
   * Until then, the workspace remains in [[Initialised]] or [[Built]] state where
   * the user is reported any validation errors in the build file
   * for that workspace.
   * */
  sealed trait Configured extends WorkspaceState {
    def build: WorkspaceBuild

    /** A workspace contains multiple source files */
    def sourceCode: ArraySeq[SourceCodeState]

    def workspaceURI: URI =
      build.workspaceURI

    override def buildOpt: Option[WorkspaceBuild] =
      Some(build)

    /** Add or update the source file */
    def updateOrAdd(newState: SourceCodeState): ArraySeq[SourceCodeState] = {
      val index = sourceCode.indexWhere(_.fileURI == newState.fileURI)
      if (index >= 0)
        sourceCode.updated(index, newState)
      else
        sourceCode appended newState
    }
  }

  sealed trait CompileRun extends WorkspaceState.Configured {
    def parsed: WorkspaceState.Parsed
  }

  /** State: IDE is initialised but the build file requires validation */
  case class Initialised(workspaceURI: URI) extends WorkspaceState {
    override def buildOpt: Option[WorkspaceBuild] =
      None
  }

  /** State: Build file is compiled. Next step is to compile the source code */
  case class Built(build: WorkspaceBuild) extends WorkspaceState {
    override def workspaceURI: URI =
      build.workspaceURI

    override def buildOpt: Option[WorkspaceBuild] =
      Some(build)
  }

  /** State: Source files might be un-compiled or partially parsed or compiled */
  case class UnCompiled(build: WorkspaceBuild,
                        sourceCode: ArraySeq[SourceCodeState]) extends WorkspaceState.Configured

  /** State: All source files parsed, therefore this workspace can be compiled */
  case class Parsed(build: WorkspaceBuild,
                    sourceCode: ArraySeq[SourceCodeState.Parsed]) extends WorkspaceState.Configured

  /**
   * Result of an errored compiler run.
   *
   * @param sourceCode      New valid source code states.
   * @param workspaceErrors Project/workspace level errors
   * @param parsed          Previous valid parsed state (used for code completion in-case the file has error)
   */
  case class Errored(sourceCode: ArraySeq[SourceCodeState],
                     workspaceErrors: ArraySeq[FormattableError],
                     parsed: WorkspaceState.Parsed) extends CompileRun {
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
                      parsed: WorkspaceState.Parsed) extends CompileRun {
    def build: WorkspaceBuild =
      parsed.build
  }

}
