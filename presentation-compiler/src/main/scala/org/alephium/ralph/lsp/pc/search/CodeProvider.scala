// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, LinePosition}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.search.completion.{CodeCompletionProvider, Suggestion}
import org.alephium.ralph.lsp.pc.search.gotodef.{GoToDefCodeProvider, GoToDefSetting}
import org.alephium.ralph.lsp.pc.search.gotodef.soft.GoToDefCodeProviderSoft
import org.alephium.ralph.lsp.pc.search.gotoref.{GoToRefCodeProvider, GoToRefSetting}
import org.alephium.ralph.lsp.pc.search.gototypedef.GoToTypeDefCodeProvider
import org.alephium.ralph.lsp.pc.search.hover.HoverCodeProvider
import org.alephium.ralph.lsp.pc.search.inlayhints.InlayHintsCodeProvider
import org.alephium.ralph.lsp.pc.search.rename.GoToRenameCodeProvider
import org.alephium.ralph.lsp.pc.search.CodeProvider.{searchWorkspace, searchWorkspaceAndDependencies}
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.URIUtil

import java.net.URI

/**
 * A trait representing a code provider that performs search operations
 * within a single workspace.
 *
 * @tparam S The type of source code state.
 * @tparam I The type of search settings or configuration.
 * @tparam O The type of search results.
 */
trait CodeProvider[S, I, O] extends Product {

  /**
   * Searches the source-code of a workspace at the given [[LinePosition]], including workspace dependencies.
   *
   * @note Use this function for all public search calls so that searches also include dependencies.
   *       [[searchLocal]] will be made protected in the future to restrict it to local-only access.
   *
   * @param linePosition   Line and character position in a document (zero-based).
   * @param fileURI        The text document's uri.
   * @param workspace      Current workspace state.
   * @param searchSettings Provider-specific settings.
   */
  final def search(
      linePosition: LinePosition,
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: I
    )(implicit provider: CodeProvider[S, I, O],
      searchCache: SearchCache,
      logger: ClientLogger): Option[Either[CompilerMessage.Error, Iterator[O]]] =
    search(
      line = linePosition.line,
      character = linePosition.character,
      fileURI = fileURI,
      workspace = workspace,
      searchSettings = searchSettings
    )

  /**
   * Searches the source-code of a workspace at the given position, including workspace dependencies.
   *
   * @note Use this function for all public search calls so that searches also include dependencies.
   *       [[searchLocal]] will be made protected in the future to restrict it to local-only access.
   *
   * @param line           Line position in a document (zero-based).
   * @param character      Character offset on a line in a document (zero-based).
   * @param fileURI        The text document's uri.
   * @param workspace      Current workspace state.
   * @param searchSettings Provider-specific settings.
   */
  final def search(
      line: Int,
      character: Int,
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: I
    )(implicit provider: CodeProvider[S, I, O],
      searchCache: SearchCache,
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
   * Searches the source-code of a workspace at the given cursor index, excluding any workspace dependencies.
   *
   * @note This function will be made protected in the future to restrict it to local-only access.
   *       Use [[search]] instead for searches that should include workspace dependencies.
   *
   * @param cursorIndex    The index (character offset) in the source code representing the cursor position.
   * @param sourceCode     The source code state where the search is executed.
   * @param workspace      The workspace state where the source-code is located.
   * @param searchSettings Provider-specific settings.
   * @return Search results.
   */
  def searchLocal(
      cursorIndex: Int,
      sourceCode: S,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: I
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[O]

}

object CodeProvider {

  /** The code-completer implementation of [[CodeProvider]]. */
  implicit val codeCompleter: CodeProvider[SourceCodeState.Parsed, Unit, Suggestion] =
    CodeCompletionProvider

  /** The go-to definition implementation of [[CodeProvider]]. */
  implicit val goToDef: CodeProvider[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict] =
    GoToDefCodeProvider

  implicit val goToDefSoft: CodeProvider[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.GoToDefSoft] =
    GoToDefCodeProviderSoft

  /** The go-to references implementation of [[CodeProvider]]. */
  implicit val goToRef: CodeProvider[SourceCodeState.Parsed, GoToRefSetting, SourceLocation.GoToRefStrict] =
    GoToRefCodeProvider

  /** The rename request implementation of [[CodeProvider]]. */
  implicit val goToRename: CodeProvider[SourceCodeState.Parsed, Unit, SourceLocation.GoToRenameStrict] =
    GoToRenameCodeProvider

  /** The go-to type implementation of [[CodeProvider]]. */
  implicit val goToTypeDef: CodeProvider[SourceCodeState.Parsed, Unit, SourceLocation.GoToTypeDef] =
    GoToTypeDefCodeProvider

  /** The inlay-hint implementation of [[CodeProvider]]. */
  implicit val inlayHints: CodeProvider[SourceCodeState.Parsed, LinePosition, SourceLocation.InlayHint] =
    InlayHintsCodeProvider

  /** The hover implementation of [[CodeProvider]]. */
  implicit val hover: CodeProvider[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.Hover] =
    HoverCodeProvider

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
      searchCache: SearchCache,
      logger: ClientLogger): Option[Either[CompilerMessage.Error, Iterator[O]]] =
    // Search on dependencies should only run for go-to definitions and hover requests. Code-completion is ignored.
    if (provider == CodeProvider.goToDef || provider == CodeProvider.goToRef || provider == CodeProvider.hover)
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
      searchCache: SearchCache,
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
            provider.searchLocal(
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
