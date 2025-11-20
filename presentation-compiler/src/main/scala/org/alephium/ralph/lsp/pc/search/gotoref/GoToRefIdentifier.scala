// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node

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
      WorkspaceSearcher
        .collectAllIsParsed(workspace)
        .flatMap {
          isParsed =>
            searchSourceCodeReferences(
              isParsed = isParsed,
              definitions = definitions,
              workspace = workspace,
              searchSettings = searchSettings
            )
        }
        .distinct
        .diff(definitions)

  /**
   * Searches all references to the given definitions across all code blocks within a single source code file.
   *
   * TODO: Performance optimisations is required here because this performs a naive linear scan
   *       over the entire workspace - every source file, every code block in each file, and every
   *       AST node within each block, and is therefore inefficient.
   *       Suggestions:
   *         - 1) Execute the search in parallel across all source files, and each code block within the source file.
   *         - 2) Introduce an inverted-index-based cache.
   *
   * @param isParsed       The source code file to search.
   * @param definitions    The definitions whose references should be searched.
   * @param workspace      The workspace state where the source-code is located.
   * @param searchSettings Provider-specific settings.
   * @return All references for the given definitions.
   */
  private def searchSourceCodeReferences(
      isParsed: SourceCodeState.IsParsed,
      definitions: ArraySeq[SourceLocation.NodeSoft[SoftAST.CodeString]],
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Seq[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (definitions.isEmpty)
      Seq.empty
    else
      isParsed.astSoft.fetch() match {
        case Right(block) =>
          // TODO: Execute in parallel.
          block.parts.flatMap {
            part =>
              // TODO: Replace `walk` with an inverted-index.
              part.toNode.walk.collect {
                case Node(ast: SoftAST.Identifier, _) if isMatchingIdentifier(ast, isParsed, definitions, workspace, searchSettings) =>
                  SourceLocation.NodeSoft(
                    ast = ast.code,
                    source = SourceLocation.CodeSoft(
                      part = part,
                      parsed = isParsed
                    )
                  )
              }
          }

        case Left(error) =>
          logger.error {
            s"""SoftParser Error: Failed to parse source code.
               |File: ${isParsed.fileURI}
               |Error Message: ${error.message}""".stripMargin
          }

          Seq.empty
      }

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
      isParsed: SourceCodeState.IsParsed,
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
