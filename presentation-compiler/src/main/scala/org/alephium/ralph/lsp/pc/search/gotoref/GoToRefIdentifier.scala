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

object GoToRefIdentifier extends StrictImplicitLogging {

  def searchLocal(
      cursorIndex: Int,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): ArraySeq[SourceLocation.GoToRefSoft] = {
    val definitions =
      CodeProvider
        .goToDef
        .search( // find definitions for the token at the given cursorIndex.
          linePosition = StringUtil.buildLineRange(sourceCode.code, cursorIndex, cursorIndex).from,
          fileURI = sourceCode.fileURI,
          workspace = workspace,
          searchSettings = (SoftAST, searchSettings.goToDefSetting)
        )
        .toList
        .flatMap {
          case Right(definitions) =>
            definitions collect {
              case SourceLocation.NodeSoft(codeString: SoftAST.CodeString, source) =>
                SourceLocation.NodeSoft(codeString, source)
            }

          case Left(_) =>
            // TODO: Report error
            ???
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

  private def searchWorkspaceReferences(
      definitions: List[SourceLocation.NodeSoft[SoftAST.CodeString]],
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): ArraySeq[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (definitions.isEmpty)
      ArraySeq.empty
    else {
      val files =
        WorkspaceSearcher.collectAllIsParsed(workspace)

      files
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
    }

  private def searchSourceCodeReferences(
      isParsed: SourceCodeState.IsParsed,
      definitions: List[SourceLocation.NodeSoft[SoftAST.CodeString]],
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Seq[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (definitions.isEmpty)
      Seq.empty
    else
      isParsed.astSoft.fetch() match {
        case Right(block) =>
          block.parts flatMap {
            part =>
              // TODO: For performance, replace `walk` with the cache `Map[Identifier, Position]`
              part.toNode.walk.collect {
                case Node(ast: SoftAST.Identifier, _) if isMatchingAST(ast, isParsed, definitions, workspace, searchSettings) =>
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

  private def isMatchingAST(
      ast: SoftAST.Identifier,
      isParsed: SourceCodeState.IsParsed,
      definitions: List[SourceLocation.NodeSoft[SoftAST.CodeString]],
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Boolean =
    if (definitions.exists(_.ast.text == ast.code.text))
      CodeProvider
        .goToDef
        .search( // find definitions for the token at the given cursorIndex.
          linePosition = ast.index.toLineRange(isParsed.code).from,
          fileURI = isParsed.fileURI,
          workspace = workspace,
          searchSettings = (SoftAST, searchSettings.goToDefSetting)
        )
        .exists {
          case Right(thisDefinition) =>
            val thisDefs = thisDefinition.toList
            val isMatch  = thisDefs exists definitions.contains
            isMatch

          case Left(error) =>
            // TODO: Report error
            ???
        }
    else
      false

}
