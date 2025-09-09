// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.inlayhints

import org.alephium.ralph.{Ast, SourceIndex}
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, LinePosition}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node

/**
 * Implements [[CodeProvider]] that provides go-to definition results of type [[SourceLocation.InlayHint]].
 *
 * To execution this function invoke [[CodeProvider.searchLocal]] with [[SourceLocation.InlayHint]] as type parameter.
 */
private[search] case object InlayHintsCodeProvider extends CodeProvider[SourceCodeState.Parsed, LinePosition, SourceLocation.InlayHint] with StrictImplicitLogging {

  /** @inheritdoc */
  override def searchLocal(
      rangeStart: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      rangeEnd: LinePosition
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.InlayHint] = {
    val eligibleNodes =
      collectHintEligibleNodesInRange(
        rangeStart = rangeStart,
        rangeEnd = rangeEnd,
        sourceCode = sourceCode
      )

    eligibleNodes
      .iterator
      .flatMap {
        node =>
          searchTypeDefinitions(
            node = node,
            sourceCode = sourceCode,
            workspace = workspace
          )
      }
      .flatMap {
        /*
         * Convert type-definitions to inlay-hints.
         */
        case (_, Some(Left(error))) =>
          logger.error(s"${InlayHintsCodeProvider.productPrefix}: ${error.message}")
          Iterable.empty

        case (sourceIndex, Some(Right(types))) =>
          // convert the types to TypeHint responses.
          types map {
            typeDef =>
              val inlayHint =
                s": ${typeDef.ast.name}"

              val position =
                SourceIndex(
                  index = sourceIndex.endIndex,
                  width = 0,
                  fileURI = None
                )

              SourceLocation.InlayHint(
                position = position,
                parsed = sourceCode,
                hint = inlayHint,
                typeDef = typeDef
              )
          }

        case (_, None) =>
          Iterable.empty
      }
  }

  /**
   * Searches type-definitions for the given variable declaration.
   *
   * @param node       The variable node to search the type for.
   * @param sourceCode The source code from which nodes are to be collected.
   * @param workspace  The workspace state where the source-code is located.
   * @return The [[SourceIndex]] of the variable and it's corresponding type-definitions.
   */
  private def searchTypeDefinitions(
      node: Node[Ast.VarDeclaration, Ast.Positioned],
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[(SourceIndex, Option[Either[CompilerMessage.Error, Iterator[SourceLocation.GoToTypeDef]]])] =
    // Collect all identifiers in the assignment.
    node
      .walk
      .collect {
        case Node(ident: Ast.Ident, _) =>
          ident.sourceIndex

        case Node(anonymousVar: Ast.AnonymousVar, _) =>
          anonymousVar.sourceIndex
      }
      .flatten
      .map {
        varSourceIndex =>
          // convert the SourceIndex to LineRange.
          val varRange =
            varSourceIndex.toLineRange(sourceCode.code)

          // Find all type definitions at the range.
          val typeDefinitions =
            CodeProvider
              .goToTypeDef
              .search(
                line = varRange.from.line,
                character = varRange.from.character,
                fileURI = sourceCode.fileURI,
                workspace = workspace,
                searchSettings = ()
              )

          (varSourceIndex, typeDefinitions)
      }

  /**
   * Collects AST nodes within a specified range that can provide type hints for the inlay-hints.
   *
   * @param rangeStart The starting index of the target range in the source code.
   * @param rangeEnd   The ending index of the target range in the source code.
   * @param sourceCode The source code from which nodes are to be collected.
   * @return All nodes within the range eligible for displaying type hints.
   */
  private def collectHintEligibleNodesInRange(
      rangeStart: Int,
      rangeEnd: LinePosition,
      sourceCode: SourceCodeState.Parsed): Seq[Node[Ast.VarDeclaration, Ast.Positioned]] = {
    val endIndex =
      StringUtil.computeIndex(
        code = sourceCode.code,
        line = rangeEnd.line,
        character = rangeEnd.character
      )

    val searchRange =
      SourceIndex(
        index = rangeStart,
        width = endIndex - rangeStart,
        fileURI = None
      )

    sourceCode
      .astStrict
      .statements
      .filter {
        statement =>
          statement.index overlaps searchRange
      }
      .flatMap {
        case _: Tree.Import =>
          // Imports don't require inlay-hints
          Iterator.empty

        case tree: Tree.Source =>
          tree
            .rootNode
            .filter(_.data.sourceIndex.exists(_ overlaps searchRange))
            .collect {
              case node @ Node(varDef: Ast.VarDeclaration, _) =>
                node.upcast(varDef)
            }
      }
  }

}
