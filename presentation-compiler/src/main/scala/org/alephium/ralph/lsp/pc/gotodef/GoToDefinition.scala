package org.alephium.ralph.lsp.pc.gotodef

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.log.StrictImplicitLogging
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.util.StringUtil
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}

import java.net.URI
import scala.collection.immutable.ArraySeq

object GoToDefinition extends StrictImplicitLogging {

  /**
   * Provides go-to definition for the cursor position within the current workspace state.
   *
   * @param line      Line position in a document (zero-based).
   * @param character Character offset on a line in a document (zero-based).
   * @param fileURI   The text document's uri.
   * @param workspace Current workspace state.
   * @return
   */
  def goTo(line: Int,
           character: Int,
           fileURI: URI,
           workspace: WorkspaceState.IsSourceAware): Option[Either[CompilerMessage.Error, ArraySeq[GoToLocation]]] =
    Workspace.findParsed(
      fileURI = fileURI,
      workspace = workspace
    ) map {
      result =>
        result map {
          parsed =>
            // fetch the requested index from line number and character number.
            val cursorIndex =
              StringUtil.computeIndex(
                lines = parsed.codeLines,
                line = line,
                character = character
              )

            goTo(
              cursorIndex = cursorIndex,
              workspace = workspace,
              sourceCode = parsed
            )
        }
    }

  private def goTo(cursorIndex: Int,
                   workspace: WorkspaceState.IsSourceAware,
                   sourceCode: SourceCodeState.Parsed): ArraySeq[GoToLocation] =
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
