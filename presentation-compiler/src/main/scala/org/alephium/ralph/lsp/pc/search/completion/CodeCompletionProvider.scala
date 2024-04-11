package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

/**
 * Implements [[CodeProvider]] that provides code completion results of type [[Suggestion]].
 *
 * To execution this function invoke [[CodeProvider.search]] with [[Suggestion]] as type parameter.
 */
private[search] object CodeCompletionProvider extends CodeProvider[Suggestion] with StrictImplicitLogging {

  /** @inheritdoc */
  override def search(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[Suggestion] =
    // find the statement where this cursorIndex sits.
    sourceCode.ast.statements.find(_.index contains cursorIndex) match {
      case Some(statement) =>
        statement match {
          case importStatement: Tree.Import =>
            // request is for import statement completion
            ImportCompleter
              .complete(
                cursorIndex = cursorIndex,
                dependency = workspace.build.findDependency(DependencyID.Std),
                imported = importStatement
              )
              .iterator

          case _: Tree.Source =>
            Iterator.empty // TODO: Provide source level completion.
        }

      case None =>
        // TODO: Provide top level completion.
        Iterator.empty
    }

}
