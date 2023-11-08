package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState.BuildCompiled
import org.alephium.ralph.lsp.pc.workspace.build.Build

import java.net.URI
import scala.collection.immutable.ArraySeq

sealed trait WorkspaceState {
  def workspaceURI: URI

  def buildURI: URI =
    Build.toBuildURI(workspaceURI)
}

object WorkspaceState {

  /** Workspace state where the source-code is known and can be parsed and compiled */
  sealed trait IsSourceAware extends WorkspaceState {
    def build: BuildCompiled

    def workspaceURI: URI =
      build.workspaceURI

    /** A workspace contains multiple source files */
    def sourceCode: ArraySeq[SourceCodeState]
  }

  /** State: Represents a compilation run result */
  sealed trait IsCompiled extends IsSourceAware {
    def parsed: WorkspaceState.Parsed
  }

  /** State: IDE is initialised but no source compilation has occurred yet */
  case class Created(workspaceURI: URI) extends WorkspaceState

  /** State: Source files might be un-compiled, parsed or compiled. This state can be parsed and compiled. */
  case class UnCompiled(build: BuildCompiled,
                        sourceCode: ArraySeq[SourceCodeState]) extends IsSourceAware

  /** State: All source files are parsed, therefore this workspace can be compiled */
  case class Parsed(build: BuildCompiled,
                    sourceCode: ArraySeq[SourceCodeState.Parsed]) extends IsSourceAware

  /**
   * Result of an errored compiler run.
   *
   * @param sourceCode      New valid source code states.
   * @param workspaceErrors Project/workspace level errors
   * @param parsed          Previous valid parsed state (used for code completion in-case the file has error)
   */
  case class Errored(sourceCode: ArraySeq[SourceCodeState],
                     workspaceErrors: ArraySeq[CompilerMessage.AnyError],
                     parsed: WorkspaceState.Parsed) extends IsCompiled {
    def build: BuildCompiled =
      parsed.build
  }

  /**
   * Result of a successful compiler run.
   *
   * @param sourceCode New valid source code states.
   * @param parsed     Current parser run for this compiled code.
   */
  case class Compiled(sourceCode: ArraySeq[SourceCodeState],
                      parsed: WorkspaceState.Parsed) extends IsCompiled {
    def build: BuildCompiled =
      parsed.build
  }

}
