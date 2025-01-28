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

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import scala.collection.immutable.ArraySeq

private object WorkspaceStateBuilder {

  /** @see [[org.alephium.ralph.lsp.pc.sourcecode.SourceCodeStateBuilder.toSourceCodeState]] */
  def toWorkspaceState(
      currentState: WorkspaceState.Parsed,
      compilationResult: Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState.IsParsedAndCompiled]]): WorkspaceState.IsCompiled =
    compilationResult match {
      case Left(workspaceError) =>
        // File or sourcePosition position information is not available for this error,
        // report it at project error.
        WorkspaceState.Errored(
          sourceCode = currentState.sourceCode,       // SourceCode remains the same as existing state
          workspaceErrors = ArraySeq(workspaceError), // errors to report
          parsed = currentState
        )

      case Right(compiledSource) =>
        val (errors, compiled) =
          compiledSource partitionMap {
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
            sourceCode = compiledSource,      // SourceCode remains the same as existing state
            workspaceErrors = ArraySeq.empty, // errors to report
            parsed = currentState
          )
        else
          WorkspaceState.Compiled(
            sourceCode = compiled,
            parsed = currentState
          )
    }

}
