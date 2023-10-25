package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.ListMap

private[workspace] object WorkspaceStateBuilder {

  def toWorkspaceState(currentState: WorkspaceState.Parsed,
                       compilationResult: Either[(ArraySeq[CompilerMessage.AnyError], Seq[SourceCodeState.ErrorSource]), ArraySeq[SourceCodeState.CodeAware]]): WorkspaceState.CompilerRun =
    compilationResult match {
      case Left((workspaceErrors, sourceCodeErrors)) =>
        // File or sourcePosition position information is not available for workspace errors,
        // report them at project error.
        WorkspaceState.Errored(
          sourceCode = mergeSourceCodeAndErrors(currentState.sourceCode, sourceCodeErrors),
          workspaceErrors = workspaceErrors, // errors to report
          parsed = currentState,
        )

      case Right(compiledSource) =>
        WorkspaceState.Compiled(
          sourceCode = compiledSource,
          parsed = currentState
        )
    }

  //Errors needs to replace initial parsed sourceCodes
  private def mergeSourceCodeAndErrors(sourceCode: Seq[SourceCodeState.Parsed], errors: Seq[SourceCodeState.ErrorSource]): ArraySeq[SourceCodeState] =  {
    //Using List map to preserve order
    val sourceCodeMap: ListMap[URI, SourceCodeState] = ListMap.from(sourceCode.map{sc => (sc.fileURI, sc)})
    ArraySeq.from(errors.foldLeft(sourceCodeMap){ case (acc, error) =>
      acc.updated(error.fileURI, error)
    }.values)
  }
}
