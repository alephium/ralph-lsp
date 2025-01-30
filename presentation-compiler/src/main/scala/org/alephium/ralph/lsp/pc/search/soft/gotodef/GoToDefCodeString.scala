package org.alephium.ralph.lsp.pc.search.soft.gotodef

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.Node

private object GoToDefCodeString {

  /**
   * Executes `go-to-definition` search for a [[Node]] representing the AST [[SoftAST.CodeString]].
   *
   * @param node       The node representing the [[SoftAST.CodeString]] being searched.
   * @param sourceCode The body part and its parsed [[org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState]],
   *                   which contains the [[SoftAST.CodeString]].
   * @return An iterator over definition search results.
   */
  def apply(
      node: Node[SoftAST.CodeString, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.GoToDefSoft] =
    node.parent match {
      case Some(node @ Node(id @ SoftAST.Identifier(_, _, _), _)) =>
        GoToDefIdentifier(
          identNode = node.upcast(id),
          sourceCode = sourceCode
        )

      case _ =>
        Iterator.empty
    }

}
