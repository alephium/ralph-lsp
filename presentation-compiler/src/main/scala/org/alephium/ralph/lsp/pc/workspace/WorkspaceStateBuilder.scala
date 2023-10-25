package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import java.net.URI
import scala.collection.immutable.ArraySeq

private[workspace] object WorkspaceStateBuilder {

  def toWorkspaceState(currentState: WorkspaceState.Parsed,
                       compilationResult: (Option[CompilerMessage.AnyError], ArraySeq[SourceCodeState.CodeAware])): WorkspaceState.CompilerRun =
    compilationResult match {
      case (Some(workspaceError), sourceCode) =>
        // File or sourcePosition position information is not available for workspace errors,
        // report them at project error.
        WorkspaceState.Errored(
          sourceCode = sourceCode,
          workspaceErrors = ArraySeq(workspaceError), // errors to report
          parsed = currentState,
        )

      case (None,compiledSource) =>
        WorkspaceState.Compiled(
          sourceCode = compiledSource,
          parsed = currentState
        )
    }
}
