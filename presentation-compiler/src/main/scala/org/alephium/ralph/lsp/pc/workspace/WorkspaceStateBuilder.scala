// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerRunResult
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeCompilerRun, SourceCodeState}

import scala.collection.immutable.ArraySeq

private object WorkspaceStateBuilder {

  /** @see [[org.alephium.ralph.lsp.pc.sourcecode.SourceCodeStateBuilder.toSourceCodeState]] */
  def toWorkspaceState(
      currentState: WorkspaceState.Parsed,
      compilationResult: Either[CompilerRunResult.Errored, SourceCodeCompilerRun]): WorkspaceState.IsCompiled =
    compilationResult match {
      case Left(workspaceError) =>
        // File or sourcePosition position information is not available for this error,
        // report it at project error.
        WorkspaceState.Errored(
          sourceCode = currentState.sourceCode, // SourceCode remains the same as existing state
          compilerRunGlobalState = workspaceError.globalState,
          workspaceErrors = ArraySeq(workspaceError.error), // errors to report
          parsed = currentState
        )

      case Right(compiledSource) =>
        val (errors, compiled) =
          compiledSource.compiledSource partitionMap {
            case error: SourceCodeState.IsParserOrCompilationError =>
              Left(error)

            case parsed: SourceCodeState.Parsed =>
              // parsed means it still contains errors, probably from Import type checking phase.
              Left(parsed)

            case compiled: SourceCodeState.Compiled =>
              Right(compiled)
          }

        if (errors.nonEmpty)
          WorkspaceState.Errored(
            sourceCode = compiledSource.compiledSource, // SourceCode remains the same as existing state
            compilerRunGlobalState = compiledSource.compilerRunGlobalState,
            workspaceErrors = ArraySeq.empty, // errors to report
            parsed = currentState
          )
        else
          WorkspaceState.Compiled(
            sourceCode = compiled,
            compilerRunGlobalState = compiledSource.compilerRunGlobalState,
            parsed = currentState
          )
    }

}
