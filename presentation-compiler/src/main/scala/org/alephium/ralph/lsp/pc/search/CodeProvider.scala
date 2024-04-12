package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.completion.{Suggestion, CodeCompletionProvider}
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefinitionProvider
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

import java.net.URI

/**
 * A trait representing a code provider, which performs search operations
 * within the source code of a workspace.
 *
 * @tparam A The type of search results.
 */
trait CodeProvider[A] {

  /**
   * Performs a search operation at the cursor index within the source-code of a workspace.
   *
   * @param cursorIndex The index location where the search operation is performed.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param workspace   The workspace state where the source-code is located.
   * @return An [[ArraySeq]] of search results of type [[A]].
   */
  def search(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[A]

}

object CodeProvider {

  /** The code-completer implementation of [[CodeProvider]]. */
  implicit val codeCompleter: CodeProvider[Suggestion] =
    CodeCompletionProvider

  /** The go-to definition implementation of [[CodeProvider]]. */
  implicit val goToDefinition: CodeProvider[GoToLocation] =
    GoToDefinitionProvider

  /**
   * Execute search at cursor position within the current workspace state.
   *
   * @param line      Line position in a document (zero-based).
   * @param character Character offset on a line in a document (zero-based).
   * @param fileURI   The text document's uri.
   * @param workspace Current workspace state.
   * @tparam A The type to search.
   */
  def search[A](
      line: Int,
      character: Int,
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware
    )(implicit provider: CodeProvider[A],
      logger: ClientLogger): Option[Either[CompilerMessage.Error, Iterator[A]]] =
    WorkspaceSearcher
      .findParsed( // find the parsed file where this search was executed.
        fileURI = fileURI,
        workspace = workspace
      )
      .map {
        parsedOpt =>
          parsedOpt map {
            parsed =>
              // fetch the requested index from line number and character number.
              val cursorIndex =
                StringUtil.computeIndex(
                  code = parsed.code,
                  line = line,
                  character = character
                )

              // execute the search
              provider.search(
                cursorIndex = cursorIndex,
                sourceCode = parsed,
                workspace = workspace
              )
          }
      }

}
