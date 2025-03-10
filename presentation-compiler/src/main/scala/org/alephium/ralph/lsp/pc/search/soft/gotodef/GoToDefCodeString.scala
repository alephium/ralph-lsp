// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.soft.gotodef

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.ClientLogger

private object GoToDefCodeString {

  /**
   * Executes `go-to-definition` search for a [[Node]] representing the AST [[SoftAST.CodeString]].
   *
   * @param node       The node representing the [[SoftAST.CodeString]] being searched.
   * @param sourceCode The body part and its parsed [[org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState]],
   *                   which contains the [[SoftAST.CodeString]].
   * @param workspace  The workspace state where the source-code is located.
   * @return An iterator over definition search results.
   */
  def apply(
      node: Node[SoftAST.CodeString, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToDefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] =
    node.parent match {
      case Some(Node(_: SoftAST.Space, _)) =>
        // Spaces do not require go-to-definition
        Iterator.empty

      case Some(node @ Node(id: SoftAST.Identifier, _)) =>
        GoToDefIdentifier(
          identNode = node.upcast(id),
          sourceCode = sourceCode,
          workspace = workspace,
          settings = settings
        )

      case _ =>
        Iterator.empty
    }

}
