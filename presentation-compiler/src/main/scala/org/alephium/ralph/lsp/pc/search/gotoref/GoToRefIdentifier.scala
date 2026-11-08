// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import scala.collection.immutable.ArraySeq

private object GoToRefIdentifier extends StrictImplicitLogging {

  /**
   * Searches for references at the given cursor position by delegating
   * to the go-to-definition provider ([[CodeProvider.goToDef]]).
   *
   * @param cursorIndex    The index (character offset) in the source code representing the cursor position.
   * @param sourceCode     The source code state where the search is executed.
   * @param workspace      The workspace state where the source-code is located.
   * @param searchSettings Provider-specific settings.
   * @return References of the token defined at the cursor position.
   */
  def search(
      cursorIndex: Int,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): ArraySeq[SourceLocation.GoToRefSoft] = {
    val definitionsOption =
      CodeProvider
        .goToDef
        .search( // find definitions for the token at the given `cursorIndex`.
          // TODO: CodeProvider converts `LinePosition` to `cursorIndex` which here gets converted back to `LinePosition`.
          //       Send `LinePosition` directly to `GoToRefSoftCodeProvider`.
          linePosition = StringUtil.buildLineRange(sourceCode.code, cursorIndex, cursorIndex).from,
          fileURI = sourceCode.fileURI,
          workspace = workspace,
          searchSettings = (SoftAST, searchSettings.goToDefSetting)
        )

    val definitions =
      definitionsOption match {
        case Some(Right(definitions)) =>
          definitions
            .collect {
              case SourceLocation.NodeSoft(codeString: SoftAST.CodeString, source) =>
                SourceLocation.NodeSoft(codeString, source)

              // TODO: Support go-to-references for import statements.
              // case _: SourceLocation.File => ???
            }
            .to(ArraySeq)

        case Some(Left(error)) =>
          logger.error {
            s"""Error: Failed goToDef search in searchDelegate.
               |File: ${sourceCode.fileURI}
               |Error Message: ${error.message}""".stripMargin
          }

          ArraySeq.empty

        case None =>
          logger.debug(s"Definition does not exist for index $cursorIndex")
          ArraySeq.empty
      }

    val references =
      searchWorkspaceReferences(
        definitions = definitions,
        workspace = workspace,
        searchSettings = searchSettings
      )

    if (searchSettings.includeDeclaration)
      references ++ definitions
    else
      references
  }

  /**
   * Searches all references to the given definitions across the entire workspace.
   *
   * @param definitions    The definitions whose references should be searched.
   * @param workspace      The workspace state where the source-code is located.
   * @param searchSettings Provider-specific settings.
   * @return All references for the given definitions.
   */
  private def searchWorkspaceReferences(
      definitions: ArraySeq[SourceLocation.NodeSoft[SoftAST.CodeString]],
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): ArraySeq[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (definitions.isEmpty)
      ArraySeq.empty
    else
      searchCache
        .get(workspace)
        .referenceIndex
        .candidates(definitions.map(_.ast.text))
        .filter {
          candidate =>
            isMatchingIdentifier(candidate.ast, candidate.source.parsed, definitions, workspace, searchSettings)
        }
        .map(
          candidate => SourceLocation.NodeSoft(candidate.ast.code, candidate.source)
        )
        .distinct
        .diff(definitions)

  /**
   * Checks whether the given identifier is a reference to any of the provided definitions.
   *
   * @param identifier     The identifier candidate to check.
   * @param isParsed       The source file that contains the identifier.
   * @param definitions    The target definitions to match the identifier against.
   * @param workspace      The workspace state where the source-code is located.
   * @param searchSettings Provider-specific settings.
   * @return `true` if the identifier's definition matches the given definitions, `false` otherwise.
   */
  private def isMatchingIdentifier(
      identifier: SoftAST.Identifier,
      isParsed: SourceCodeState.IsParsedAndCompiled,
      definitions: ArraySeq[SourceLocation.NodeSoft[SoftAST.CodeString]],
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Boolean =
    if (definitions.exists(_.ast.text == identifier.code.text))
      CodeProvider
        .goToDef
        .search( // find definitions for the token at the given cursorIndex.
          linePosition = identifier.index.toLineRange(isParsed.code).from,
          fileURI = isParsed.fileURI,
          workspace = workspace,
          searchSettings = (SoftAST, searchSettings.goToDefSetting)
        )
        .exists {
          case Right(thisDefinition) =>
            thisDefinition exists definitions.contains

          case Left(error) =>
            logger.error {
              s"""Error: Failed to search goToDef in isMatchingAST.
                 |File: ${isParsed.fileURI}
                 |Error Message: ${error.message}""".stripMargin
            }

            false
        }
    else
      false

}
