package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import scala.collection.immutable.ArraySeq

/**
 * Implements [[CodeProvider]] that provides go-to definition results of type [[GoToLocation]].
 *
 * To execution this function invoke [[CodeProvider.search]] with [[GoToLocation]] as type parameter.
 */
private[search] object GoToDefinitionProvider extends CodeProvider[GoToLocation] with StrictImplicitLogging {

  override def search(cursorIndex: Int,
                      sourceCode: SourceCodeState.Parsed,
                      workspace: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): ArraySeq[GoToLocation] =
    // find the statement where this cursorIndex sits.
    sourceCode.ast.statements.find(_.index contains cursorIndex) match {
      case Some(statement) =>
        statement match {
          case importStatement: Tree.Import =>
            // request is for import go-to definition
            GoToImport.goTo(
              cursorIndex = cursorIndex,
              dependency = workspace.build.dependency,
              importStatement = importStatement
            )

          case source: Tree.Source =>
            // request is for source-code go-to definition
            GoToSource.goTo(
              cursorIndex = cursorIndex,
              sourceCode = sourceCode,
              sourceAST = source
            )
        }

      case None =>
        ArraySeq.empty
    }
}
