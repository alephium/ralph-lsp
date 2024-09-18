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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

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
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] =
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
              workspace = workspace
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
