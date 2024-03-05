package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.completion.{CodeCompleter, Suggestion}
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefinition
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.util.StringUtil
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}

import java.net.URI
import scala.collection.immutable.ArraySeq

trait CodeSearcher[A] {

  /**
   * Performs a search operation at the cursor index within the source-code of a workspace.
   *
   * @param cursorIndex The index location where the search operation is performed.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param workspace   The workspace state where the source-code is located.
   * @return An [[ArraySeq]] of search results of type [[A]].
   */
  def search(cursorIndex: Int,
             sourceCode: SourceCodeState.Parsed,
             workspace: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): ArraySeq[A]
}

object CodeSearcher {

  /** The code-completer implementation of [[CodeSearcher]] */
  implicit val codeCompleter: CodeSearcher[Suggestion] =
    CodeCompleter

  /** The go-to definition implementation of [[CodeSearcher]] */
  implicit val goToDefinition: CodeSearcher[GoToLocation] =
    GoToDefinition

  /**
   * Execute search at cursor position within the current workspace state.
   *
   * @param line      Line position in a document (zero-based).
   * @param character Character offset on a line in a document (zero-based).
   * @param fileURI   The text document's uri.
   * @param workspace Current workspace state.
   * @tparam A The type to search.
   */
  def search[A](line: Int,
                character: Int,
                fileURI: URI,
                workspace: WorkspaceState.IsSourceAware)(implicit searcher: CodeSearcher[A],
                                                         logger: ClientLogger): Option[Either[CompilerMessage.Error, ArraySeq[A]]] =
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

            searcher.search(
              cursorIndex = cursorIndex,
              sourceCode = parsed,
              workspace = workspace
            )
        }
    }

}
