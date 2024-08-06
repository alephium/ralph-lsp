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

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeSearcher}
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node

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
      sourceCode: SourceLocation.Code,
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
              Suggestion.EnumFields(SourceLocation.Node(enumDef, sourceCode))
          }
      }
  }

}
