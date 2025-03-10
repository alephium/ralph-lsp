// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.utils.Node

object EnumFieldCompleter {

  /**
   * Suggests enum fields for an enum definition with the given enum type ID.
   *
   * @param enumId     The enum type ID for which to search and suggest fields.
   * @param sourceCode The source code where this completion is requested.
   * @param workspace  The workspace state containing the source code.
   * @return An iterator over code completion suggestions.
   */
  def suggest(
      enumId: Ast.TypeId,
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.EnumFields] = {
    val trees =
      WorkspaceSearcher.collectInheritedParents(
        sourceCode = sourceCode,
        workspace = workspace
      )

    val globalEnumTrees =
      SourceCodeSearcher.collectGlobalEnumsCode(trees.allTrees.iterator)

    val allTrees =
      trees.parentTrees ++ globalEnumTrees

    allTrees
      .iterator
      .flatMap {
        sourceCode =>
          sourceCode.tree.rootNode.walkDown.collect {
            case Node(enumDef: ralph.Ast.EnumDef[_], _) if enumDef.id == enumId =>
              Suggestion.EnumFields(SourceLocation.NodeStrict(enumDef, sourceCode))
          }
      }
  }

}
