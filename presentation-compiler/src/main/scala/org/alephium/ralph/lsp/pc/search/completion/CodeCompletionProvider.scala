package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import scala.collection.immutable.ArraySeq

/**
 * Implements [[CodeProvider]] that provides code completion results of type [[Suggestion]].
 */
private[search] object CodeCompletionProvider extends CodeProvider[Suggestion] with StrictImplicitLogging {

  override def search(cursorIndex: Int,
                      sourceCode: SourceCodeState.Parsed,
                      workspace: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): ArraySeq[Suggestion] =
    // find the statement where this cursorIndex sits.
    sourceCode.ast.statements.find(_.index contains cursorIndex) match {
      case Some(statement) =>
        statement match {
          case importStatement: Tree.Import =>
            // request is for import statement completion
            ImportCompleter.complete(
              cursorIndex = cursorIndex,
              dependency = workspace.build.dependency,
              imported = importStatement
            )

          case _: Tree.Source =>
            ArraySeq.empty // TODO: Provide source level completion.
        }

      case None =>
        // TODO: Provide top level completion.
        ArraySeq.empty
    }
}
