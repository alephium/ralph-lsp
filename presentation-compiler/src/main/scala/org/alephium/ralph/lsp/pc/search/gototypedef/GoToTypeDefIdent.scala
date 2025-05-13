// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gototypedef

import org.alephium.ralph.{Ast, Type}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node

import scala.collection.immutable.ArraySeq

private case object GoToTypeDefIdent extends StrictImplicitLogging {

  /**
   * Searches type-definitions given the identifier node [[Ast.Ident]].
   *
   * @param node      The node representing the identifier being searched.
   * @param workspace The workspace state where the source-code is located.
   * @return An iterator over type-definition search results.
   */
  def apply(
      node: Node[Ast.Ident, Ast.Positioned],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToTypeDef] =
    node.parent match {
      case Some(Node(variable: Ast.Variable[_], _)) =>
        searchCachedType(
          cachedType = variable.getCachedType(),
          workspace = workspace
        )

      case Some(node @ Node(_: Ast.NamedVar, _)) =>
        node.parent match {
          case Some(Node(varDef: Ast.VarDef[_], _)) =>
            searchCachedType(
              cachedType = varDef.value.getCachedType(),
              workspace = workspace
            )

          case _ =>
            ArraySeq.empty
        }

      case Some(Node(data, _)) =>
        logger.info(s"${this.productPrefix} not implemented for ${data.getClass.getName}. SourceIndex: ${data.sourceIndex}")
        ArraySeq.empty

      case None =>
        logger.info(s"${this.productPrefix}: Type information not found for node: ${node.data.getClass.getName}. SourceIndex: ${node.data.sourceIndex}")
        ArraySeq.empty
    }

  private def searchCachedType(
      cachedType: Option[Seq[Type]],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToTypeDef] =
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
      workspace: WorkspaceState.IsSourceAware): ArraySeq[SourceLocation.GoToTypeDef] = {
    val workspaceTrees =
      WorkspaceSearcher.collectAllTrees(workspace)

    val result =
      SourceCodeSearcher.collectTypes(
        types = variableTypes,
        workspaceSource = workspaceTrees
      )

    result map {
      case (typeId, code) =>
        SourceLocation.GoToTypeDef(
          ast = typeId,
          source = code
        )
    }
  }

}
