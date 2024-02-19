package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.util.StringUtil
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}

import java.net.URI
import scala.collection.immutable.ArraySeq

object CodeCompleter extends StrictImplicitLogging {

  /**
   * Provides code completion for the cursor position within the current workspace state.
   *
   * @param line      Line position in a document (zero-based).
   * @param character Character offset on a line in a document (zero-based).
   * @param fileURI   The text document's uri.
   * @param workspace Current workspace state.
   * @return
   */
  def complete(line: Int,
               character: Int,
               fileURI: URI,
               workspace: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): Option[Either[CompilerMessage.Error, ArraySeq[Suggestion]]] =
    Workspace.findParsed(
      fileURI = fileURI,
      workspace = workspace
    ) map {
      result =>
        result map {
          parsed =>
            complete(
              line = line,
              character = character,
              workspace = workspace,
              sourceCode = parsed
            )
        }
    }

  private def complete(line: Int,
                       character: Int,
                       workspace: WorkspaceState.IsSourceAware,
                       sourceCode: SourceCodeState.Parsed)(implicit logger: ClientLogger): ArraySeq[Suggestion] = {
    // fetch the requested index from line number and character number.
    val cursorIndex =
      StringUtil.computeIndex(
        lines = sourceCode.codeLines,
        line = line,
        character = character
      )

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
}
