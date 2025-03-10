// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

private object GoToDefSource extends StrictImplicitLogging {

  /**
   * Navigates to the definition of a token in the source code.
   *
   * @param cursorIndex The index of the token selected.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param workspace   The workspace where this search was executed and where all the source trees exist.
   * @return An iterator over the target go-to location(s).
   */
  def goTo(
      cursorIndex: Int,
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToDefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    sourceCode.tree.rootNode.findLast(_.sourceIndex.exists(_ contains cursorIndex)) match { // find the node closest to this source-index
      case Some(closest) =>
        closest match {
          case identNode @ Node(ident: Ast.Ident, _) =>
            // the selected/closest node is an ident
            GoToDefIdent.goTo(
              identNode = identNode.upcast(ident),
              sourceCode = sourceCode,
              workspace = workspace
            )

          case funcIdNode @ Node(funcId: Ast.FuncId, _) =>
            // the selected/closest node is functionId
            GoToDefFuncId.goTo(
              funcIdNode = funcIdNode.upcast(funcId),
              sourceCode = sourceCode,
              workspace = workspace,
              settings = settings
            )

          case typIdNode @ Node(typeId: Ast.TypeId, _) =>
            // the selected/closest node is TypeId
            GoToDefTypeId.goTo(
              typeIdNode = typIdNode.upcast(typeId),
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(ast, _) =>
            logger.trace(s"No GoToDef implementation for '${ast.getClass.getSimpleName}'")
            Iterator.empty
        }

      case None =>
        logger.trace(s"Closest node not found for cursor index '$cursorIndex' source '${sourceCode.parsed.fileURI}'")
        Iterator.empty
    }

}
