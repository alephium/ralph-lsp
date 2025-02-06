package org.alephium.ralph.lsp.pc.search.soft.gotodef

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.Node

private object GoToDefIdentifier {

  /**
   * Searches for definitions given the location of the identifier node [[SoftAST.Identifier]]
   * and the [[SourceLocation]] of the identifier.
   *
   * Steps:
   *  - First, checks if the current [[SoftAST.Identifier]] itself belongs to a definition.
   *  - Second, executes search for all nodes within the scope of the current block of code.
   *
   * @param identNode  The node representing the identifier being searched.
   * @param sourceCode The body-part and its source code state where this search is executed.
   * @return An iterator over definition search results.
   */
  def apply(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.GoToDefSoft] =
    identNode.parent match {
      case Some(Node(_: SoftAST.ReferenceCall | _: SoftAST.DotCall, _)) =>
        // TODO: Handle function and dot calls
        Iterator.empty

      case _ =>
        // Everything else
        // TODO: Also execute inherited variables and arguments here, possibly in parallel.
        GoToDefLocalVariableDeclaration(
          identNode = identNode,
          sourceCode = sourceCode
        )
    }

}
