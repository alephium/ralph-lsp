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
   * Workspace state with successfully configured config file.
   *
   * Parsing and compilation is implemented only for these types.
   * Until then, the workspace remains in [[UnConfigured]] state where
   * the user is reported any validation errors in the ralphc-configuration file
   * for that workspace.
   * */
  sealed trait Configured extends WorkspaceState {
    def build: WorkspaceBuild

    /** A workspace contains multiple source files */
    def sourceCode: ArraySeq[SourceCodeState]

    def workspaceURI: URI =
      build.workspaceURI

    /** Add or update the source file */
    def updateOrAdd(newState: SourceCodeState): ArraySeq[SourceCodeState] = {
      val index = sourceCode.indexWhere(_.fileURI == newState.fileURI)
      if (index >= 0)
        sourceCode.updated(index, newState)
      else
        sourceCode appended newState
    }
  }

  /** State: IDE is initialised but the ralphc/workspace configuration file requires validation */
  case class UnConfigured(workspaceURI: URI) extends WorkspaceState

  /** State: Source files are un-compiled or partially-compiled */
  case class UnCompiled(build: WorkspaceBuild,
                        sourceCode: ArraySeq[SourceCodeState]) extends WorkspaceState.Configured

  /** State: All source files parsed, therefore can be compiled */
  case class Parsed(build: WorkspaceBuild,
                    sourceCode: ArraySeq[SourceCodeState.Parsed]) extends WorkspaceState.Configured

  /**
   * Result of a compilation run.
   *
   * @param sourceCode      New valid source code states.
   * @param workspaceErrors Project/workspace level errors
   * @param parsed   Previous valid parsed state
   */
  case class Compiled(sourceCode: ArraySeq[SourceCodeState],
                      workspaceErrors: ArraySeq[FormattableError],
                      parsed: WorkspaceState.Parsed) extends WorkspaceState.Configured {
    def build: WorkspaceBuild =
      parsed.build
  }

}
