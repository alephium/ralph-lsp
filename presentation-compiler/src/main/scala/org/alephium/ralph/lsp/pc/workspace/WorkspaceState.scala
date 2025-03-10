// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.typescript.TSBuild
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}

import java.net.URI
import scala.collection.immutable.ArraySeq

sealed trait WorkspaceState {

  def workspaceURI: URI

  final def buildURI: URI =
    Build.toBuildFile(workspaceURI)

  final def tsBuildURI: URI =
    TSBuild.toBuildFile(workspaceURI)

}

object WorkspaceState {

  /** Workspace state where the source-code is known and can be parsed and compiled */
  sealed trait IsSourceAware extends WorkspaceState {

    def build: BuildState.Compiled

    /** A workspace contains multiple source files */
    def sourceCode: ArraySeq[SourceCodeState]

    final def workspaceURI: URI =
      build.workspaceURI

  }

  /** Represents the results of a parser run */
  sealed trait IsParsed extends IsSourceAware

  /** Represents the results of a parser run and then a compilation run. */
  sealed trait IsParsedAndCompiled extends IsSourceAware

  /** State: Represents a parse and compilation run result */
  sealed trait IsCompiled extends IsParsedAndCompiled {

    def parsed: WorkspaceState.Parsed

    final def build: BuildState.Compiled =
      parsed.build

  }

  /** State: IDE is initialised but no source compilation has occurred yet */
  case class Created(workspaceURI: URI) extends WorkspaceState

  /** State: Source files might be un-compiled, parsed or compiled. This state can be parsed and compiled. */
  case class UnCompiled(
      build: BuildState.Compiled,
      sourceCode: ArraySeq[SourceCodeState])
    extends IsParsed
       with IsParsedAndCompiled

  /** State: All source files are parsed, therefore this workspace can be compiled */
  case class Parsed(
      build: BuildState.Compiled,
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
      sourceCode: ArraySeq[SourceCodeState.IsParsedAndCompiled],
      workspaceErrors: ArraySeq[CompilerMessage.AnyError],
      parsed: WorkspaceState.Parsed)
    extends IsCompiled

  /**
   * Result of a successful compiler run.
   *
   * @param sourceCode New valid source code states.
   * @param parsed     Current parser run for this compiled code.
   */
  case class Compiled(
      sourceCode: ArraySeq[SourceCodeState.Compiled],
      parsed: WorkspaceState.Parsed)
    extends IsCompiled

}
