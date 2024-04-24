// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.completion.{Suggestion, CodeCompletionProvider}
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefinitionProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
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
   * @return An iterator over search results of type [[A]].
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
  implicit val goToDefinition: CodeProvider[SourceLocation.GoTo] =
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
