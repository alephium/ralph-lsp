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

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

private object GoToRefSource extends StrictImplicitLogging {

  def goTo(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      isIncludeDeclaration: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] =
    // request is for source-code go-to definition
    CodeProvider
      .goToDefinition
      .search(
        cursorIndex = cursorIndex,
        sourceCode = sourceCode,
        workspace = workspace,
        searchSettings = ()
      )
      .flatMap {
        case SourceLocation.File(_) =>
          Iterator.empty

        case location @ SourceLocation.Node(_, _) =>
          goTo(
            defLocation = location,
            workspace = workspace,
            isIncludeDeclaration = isIncludeDeclaration
          )
      }

  def goTo(
      defLocation: SourceLocation.Node[Ast.Positioned],
      workspace: WorkspaceState.IsSourceAware,
      isIncludeDeclaration: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] = {
    val defNode =
      defLocation
        .source
        .tree
        .rootNode
        .walkDown
        .find(_.data eq defLocation.ast)

    defNode match {
      case Some(defNode) =>
        GoToRefNode.goTo(
          definition = defNode,
          sourceCode = defLocation.source,
          workspace = workspace,
          isIncludeDeclaration = isIncludeDeclaration
        )

      case None =>
        logger.trace(s"Node not found for AST '${defLocation.ast.getClass.getSimpleName}' at source index '${defLocation.ast.sourceIndex}'")
        Iterator.empty
    }
  }

}