package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.Build
import org.alephium.ralph.lsp.pc.workspace.build.BuildState.BuildCompiled

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

  /** Represents the results of a parser run */
  sealed trait IsParsed extends IsSourceAware

  /** Represents the results of a parser run and then a compilation run. */
  sealed trait IsParsedAndCompiled extends IsSourceAware

  /** State: Represents a parse and compilation run result */
  sealed trait IsCompiled extends IsParsedAndCompiled {

    def parsed: WorkspaceState.Parsed

  }

  /** State: IDE is initialised but no source compilation has occurred yet */
  case class Created(workspaceURI: URI) extends WorkspaceState

  /** State: Source files might be un-compiled, parsed or compiled. This state can be parsed and compiled. */
  case class UnCompiled(
      build: BuildCompiled,
      sourceCode: ArraySeq[SourceCodeState])
    extends IsParsed
       with IsParsedAndCompiled

  /** State: All source files are parsed, therefore this workspace can be compiled */
  case class Parsed(
      build: BuildCompiled,
      sourceCode: ArraySeq[SourceCodeState.Parsed])
    extends IsParsed

  /**
   * Result of an errored compiler run.
   *
   * @param sourceCode      New valid source code states.
   * @param workspaceErrors Project/workspace level errors
   * @param parsed          Previous valid parsed state (used for code completion in-case the file has error)
   */
  case class Errored(
      sourceCode: ArraySeq[SourceCodeState.IsParsed],
      workspaceErrors: ArraySeq[CompilerMessage.AnyError],
      parsed: WorkspaceState.Parsed)
    extends IsCompiled {

    def build: BuildCompiled =
      parsed.build

  }

  /**
   * Result of a successful compiler run.
   *
   * @param sourceCode New valid source code states.
   * @param parsed     Current parser run for this compiled code.
   */
  case class Compiled(
      sourceCode: ArraySeq[SourceCodeState.Compiled],
      parsed: WorkspaceState.Parsed)
    extends IsCompiled {

    def build: BuildCompiled =
      parsed.build

  }

}
