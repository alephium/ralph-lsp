// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gototypedef

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.Ast

case object GoToTypeDefCodeProvider extends CodeProvider[SourceCodeState.Parsed, Unit, SourceLocation.GoToTypeDef] with StrictImplicitLogging {

  /**
   * Performs a search operation at the cursor index within the source-code of a workspace.
   *
   * @param cursorIndex    The index (character offset) in the source code representing the cursor position.
   * @param sourceCode     The source code state where the search is executed.
   * @param workspace      The workspace state where the source-code is located.
   * @param searchSettings Provider-specific settings.
   * @return Search results.
   */
  override def search(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: Unit
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToTypeDef] =
    sourceCode.astStrict.statements.find(_.index contains cursorIndex) match {
      case Some(statement) =>
        statement match {
          case _: Tree.Import =>
            Iterator.empty

          case tree: Tree.Source =>
            tree.rootNode.findLast(_.sourceIndex.exists(_ contains cursorIndex)) match {
              case Some(node @ Node(ident: Ast.Ident, _)) =>
                GoToTypeDefIdent(
                  node = node.upcast(ident),
                  workspace = workspace
                ).iterator

              case Some(Node(data, _)) =>
                logger.error(s"GoToType not implemented for ${data.getClass.getName}. Index: $cursorIndex. FileURI: ${sourceCode.fileURI}")
                Iterator.empty

              case None =>
                logger.info(s"Type information not found at index: $cursorIndex. FileURI: ${sourceCode.fileURI}")
                Iterator.empty
            }
        }

      case None =>
        Iterator.empty
    }

}
