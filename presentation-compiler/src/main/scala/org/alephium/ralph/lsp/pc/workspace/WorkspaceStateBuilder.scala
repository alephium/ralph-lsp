package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import scala.collection.immutable.ArraySeq

private[workspace] object WorkspaceStateBuilder {

  def toWorkspaceState(currentState: WorkspaceState.Parsed,
                       compilationResult: Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState.CodeAware]]): WorkspaceState.CompilerRun =
    compilationResult match {
      case Left(workspaceError) =>
        // File or sourcePosition position information is not available for this error,
        // report it at project error.
        WorkspaceState.Errored(
          sourceCode = currentState.sourceCode, // SourceCode remains the same as existing state
          workspaceErrors = ArraySeq(workspaceError), // errors to report
          parsed = currentState,
        )

      case Right(compiledSource) =>
        WorkspaceState.Compiled(
          sourceCode = compiledSource,
          parsed = currentState
        )
    }
}
