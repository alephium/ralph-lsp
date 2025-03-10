// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.search.completion.{CodeCompletionProvider, Suggestion}
import org.alephium.ralph.lsp.pc.search.gotodef.{GoToDefinitionProvider, GoToDefSetting}
import org.alephium.ralph.lsp.pc.search.gotoref.{GoToReferenceProvider, GoToRefSetting}
import org.alephium.ralph.lsp.pc.search.rename.GoToRenameProvider
import org.alephium.ralph.lsp.pc.search.soft.gotodef.GoToDefinitionProviderSoft
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.URIUtil

import java.net.URI

/**
 * A trait representing a code provider, which performs search operations
 * within the source code of a workspace.
 *
 * @tparam I The type of search settings.
 * @tparam O The type of search results.
 */
trait CodeProvider[S, I, O] extends Product {

  /**
   * Performs a search operation at the cursor index within the source-code of a workspace.
   *
   * @param cursorIndex The index location where the search operation is performed.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param workspace   The workspace state where the source-code is located.
   * @return An iterator over search results of type [[O]].
   */
  def search(
      cursorIndex: Int,
      sourceCode: S,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: I
    )(implicit logger: ClientLogger): Iterator[O]

}

object CodeProvider {

  /** The code-completer implementation of [[CodeProvider]]. */
  implicit val codeCompleter: CodeProvider[SourceCodeState.Parsed, Unit, Suggestion] =
    CodeCompletionProvider

  /** The go-to definition implementation of [[CodeProvider]]. */
  implicit val goToDefinition: CodeProvider[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict] =
    GoToDefinitionProvider

  implicit val softGoToDefinition: CodeProvider[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.GoToDefSoft] =
    GoToDefinitionProviderSoft

  /** The go-to references implementation of [[CodeProvider]]. */
  implicit val goToReferences: CodeProvider[SourceCodeState.Parsed, GoToRefSetting, SourceLocation.GoToRefStrict] =
    GoToReferenceProvider

  /** The rename request implementation of [[CodeProvider]]. */
  implicit val goToRename: CodeProvider[SourceCodeState.Parsed, Unit, SourceLocation.GoToRenameStrict] =
    GoToRenameProvider

  /**
   * Execute search at cursor position within the current workspace state.
   *
   * @param line      Line position in a document (zero-based).
   * @param character Character offset on a line in a document (zero-based).
   * @param fileURI   The text document's uri.
   * @param workspace Current workspace state.
   * @tparam O The type to search.
   */
  def search[S, I, O](
      line: Int,
      character: Int,
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: I
    )(implicit provider: CodeProvider[S, I, O],
      logger: ClientLogger): Option[Either[CompilerMessage.Error, Iterator[O]]] =
    // if the fileURI belongs to the workspace, then search just within that workspace
    if (URIUtil.contains(workspace.build.contractURI, fileURI))
      searchWorkspace[S, I, O](
        line = line,
        character = character,
        fileURI = fileURI,
        workspace = workspace,
        searchSettings = searchSettings
      )
    else // else search all source files
      searchWorkspaceAndDependencies[S, I, O](
        line = line,
        character = character,
        fileURI = fileURI,
        workspace = workspace,
        searchSettings = searchSettings
      )

  /**
   * Executes search on dependencies and the workspace that can use this dependency.
   *
   * @param character Character offset on a line in a document (zero-based).
   * @param fileURI   The text document's uri.
   * @param workspace Current workspace state.
   * @tparam O The type to search.
   */
  private def searchWorkspaceAndDependencies[S, I, O](
      line: Int,
      character: Int,
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: I
    )(implicit provider: CodeProvider[S, I, O],
      logger: ClientLogger): Option[Either[CompilerMessage.Error, Iterator[O]]] =
    // Search on dependencies should only run for go-to definitions requests. Code-completion is ignored.
    if (provider == CodeProvider.goToDefinition || provider == CodeProvider.goToReferences)
      workspace
        .build
        .dependencies
        .find { // find the dependency where this search was executed
          dependency =>
            URIUtil.contains(dependency.build.contractURI, fileURI)
        }
        .flatMap {
          dependency =>
            // merge all source files of all dependencies, because dependencies themselves could be interdependent.
            val dependencySourceCode =
              workspace
                .build
                .dependencies
                .flatMap(_.sourceCode)

            // merge all dependencies and workspace source-files.
            val mergedSourceCode =
              workspace.sourceCode ++ dependencySourceCode

            // create one workspace with all source-code.
            val mergedWorkspace =
              WorkspaceState.UnCompiled(
                build = dependency.build,
                sourceCode = mergedSourceCode
              )

            // execute search on that one workspace
            searchWorkspace[S, I, O](
              line = line,
              character = character,
              fileURI = fileURI,
              workspace = mergedWorkspace,
              searchSettings = searchSettings
            )
        }
    else
      None

  /**
   * Execute search at cursor position within the current workspace state.
   *
   * @param line      Line position in a document (zero-based).
   * @param character Character offset on a line in a document (zero-based).
   * @param fileURI   The text document's uri.
   * @param workspace Current workspace state.
   * @tparam O The type to search.
   */
  private def searchWorkspace[S, I, O](
      line: Int,
      character: Int,
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: I
    )(implicit provider: CodeProvider[S, I, O],
      logger: ClientLogger): Option[Either[CompilerMessage.Error, Iterator[O]]] =
    getParsedStateForCodeProvider(
      fileURI = fileURI,
      workspace = workspace,
      searchSettings = searchSettings
    ) map {
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
              sourceCode = parsed.asInstanceOf[S],
              workspace = workspace,
              searchSettings = searchSettings
            )
        }
    }

  /**
   * Finds the parsed state of the given file URI, based on the type of code provider being executed.
   *
   * This function is temporary until [[SoftAST]] fully replaces StrictAST within the [[CodeProvider]]s.
   *
   * @param fileURI        The URI of the file whose parsed state is to be fetched.
   * @param workspace      The current workspace state.
   * @param searchSettings The search settings for this request.
   * @tparam I The type of the search settings used by the provider.
   * @return The current parsed state of the FileURI.
   */
  private def getParsedStateForCodeProvider[I](
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: I): Option[Either[CompilerMessage.Error, SourceCodeState.IsParsed]] =
    searchSettings match {
      case (SoftAST, _) => // This is a SoftParser, fetch the IsParsed
        WorkspaceSearcher.findIsParsed(
          fileURI = fileURI,
          workspace = workspace
        )

      case _ => // This is a StrictParser, fetch the Parsed
        WorkspaceSearcher.findParsed(
          fileURI = fileURI,
          workspace = workspace
        )
    }

}
