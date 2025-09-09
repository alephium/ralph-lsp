// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node

import scala.annotation.tailrec

case object GoToDefCodeProvider extends CodeProvider[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.GoToDefSoft] with StrictImplicitLogging {

  /** @inheritdoc */
  override def searchLocal(
      cursorIndex: Int,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: (SoftAST.type, GoToDefSetting)
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] =
    sourceCode.astSoft.fetch() match {
      case Left(error) =>
        // This will be removed when integration is complete,
        // when SourceCodeState.ErrorParser responds to SoftParser errors.
        // Note: SoftParser is not expected to fail given any input, so this is less likely to occur.
        //       Log it for now.
        logger.error {
          s"""SoftParser Error: Failed to parse source code.
             |File: ${sourceCode.fileURI}
             |Error Message: ${error.message}""".stripMargin
        }

        Iterator.empty

      case Right(softAST) =>
        // First, find the first code block where the cursorIndex belongs, i.e. [[SoftAST.BodyPartAST]].
        // In a well-defined code, this is expected to be a top level Contract [[SoftAST.Template]].
        softAST.toNode.data.parts.findLast(_.index contains cursorIndex) match {
          case Some(blockPart) =>
            searchBlockPart(
              cursorIndex = cursorIndex,
              blockPart = blockPart,
              sourceCode = sourceCode,
              workspace = workspace,
              settings = searchSettings._2,
              allowLeftShift = true
            )

          case None =>
            Iterator.empty
        }
    }

  /**
   * Searches the given bodyPart.
   *
   * @param cursorIndex    The index location where the search operation is performed.
   * @param blockPart      The first code block where the search is executed.
   * @param sourceCode     The parsed state of the source-code where the search is executed.
   * @param workspace      The workspace state where the source-code is located.
   * @param allowLeftShift If true, and the cursor is at the end of a code string, this flag
   *                       allows shifting the cursor left by one position to include the last
   *                       character in the search. Used to avoid searching the tail space or token
   *                       which would produce no result.
   */
  @tailrec
  private def searchBlockPart(
      cursorIndex: Int,
      blockPart: SoftAST.BlockPartAST,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToDefSetting,
      allowLeftShift: Boolean
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] =
    blockPart.toNode.findLast(_.data.index contains cursorIndex) match {
      case Some(Node(token: SoftAST.CodeToken[_], _)) =>
        if (allowLeftShift && token.index.from == cursorIndex)
          searchBlockPart(
            cursorIndex = cursorIndex - 1,
            blockPart = blockPart,
            sourceCode = sourceCode,
            workspace = workspace,
            settings = settings,
            allowLeftShift = false // already shifted once, disable shifting again
          )
        else
          Iterator.empty // Tokens (fn, Contract etc.) do not require go-to-definitions

      case Some(node @ Node(_: SoftAST.CodeString, _)) =>
        node.parent match {
          case Some(Node(space: SoftAST.Space, _)) =>
            if (allowLeftShift && space.index.from == cursorIndex)
              searchBlockPart(
                cursorIndex = cursorIndex - 1,
                blockPart = blockPart,
                sourceCode = sourceCode,
                workspace = workspace,
                settings = settings,
                allowLeftShift = false // already shifted once, disable shifting again
              )
            else
              Iterator.empty // Spaces do not require go-to-definition

          case Some(node @ Node(id: SoftAST.Identifier, _)) =>
            GoToDefIdentifier(
              identNode = node.upcast(id),
              sourceCode = SourceLocation.CodeSoft(blockPart, sourceCode),
              workspace = workspace,
              settings = settings
            )

          case Some(node @ Node(path: SoftAST.Path, _)) =>
            GoToDefImport(
              cursorIndex = cursorIndex,
              path = node.upcast(path),
              dependency = workspace.build.findDependency(DependencyID.Std)
            ).iterator

          case Some(Node(string: SoftAST.StringLiteral, _)) =>
            GoToDefImport(
              cursorIndex = cursorIndex,
              string = string,
              dependency = workspace.build.findDependency(DependencyID.Std)
            ).iterator

          case _ =>
            Iterator.empty
        }

      case Some(node) =>
        logger.trace(s"Go-to-definition not implemented for AST '${node.data.getClass.getSimpleName}' at source index '${node.data.index}'. File: ${sourceCode.fileURI}")
        Iterator.empty

      case None =>
        // If this occurs, the client must be sending requests with incorrect 'cursorIndex'.
        logger.error(s"AST Node not found for index $cursorIndex. File: ${sourceCode.fileURI}")
        Iterator.empty
    }

}
