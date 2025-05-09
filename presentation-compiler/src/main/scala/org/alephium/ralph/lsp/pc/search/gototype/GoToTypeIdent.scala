// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gototype

import org.alephium.ralph.{Ast, Type}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node

import scala.collection.immutable.ArraySeq

private object GoToTypeIdent extends StrictImplicitLogging {

  def apply(
      node: Node[Ast.Ident, Ast.Positioned],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToType] =
    node.parent match {
      case Some(Node(variable: Ast.Variable[_], _)) =>
        searchCachedType(
          cachedType = variable.getCachedType(),
          workspace = workspace
        )

      case Some(Node(data, _)) =>
        logger.info(s"Type-inference not implemented for ${data.getClass.getSimpleName}. SourceIndex: ${data.sourceIndex}")
        ArraySeq.empty

      case None =>
        logger.info(s"Type information not found for node: ${node.data.getClass.getSimpleName}. SourceIndex: ${node.data.sourceIndex}")
        ArraySeq.empty
    }

  private def searchCachedType(
      cachedType: Option[Seq[Type]],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToType] =
    cachedType match {
      case Some(variableTypes) =>
        searchTypes(
          variableTypes = variableTypes,
          workspace = workspace
        )

      case None =>
        logger.info("Type information not found in node's AST")
        ArraySeq.empty
    }

  private def searchTypes(
      variableTypes: Seq[Type],
      workspace: WorkspaceState.IsSourceAware): ArraySeq[SourceLocation.GoToType] = {
    val workspaceTrees =
      WorkspaceSearcher.collectAllTrees(workspace)

    val result =
      SourceCodeSearcher.collectTypes(
        types = variableTypes,
        workspaceSource = workspaceTrees
      )

    result map {
      case (typeId, code) =>
        SourceLocation.GoToType(
          ast = typeId,
          source = code
        )
    }
  }

}
