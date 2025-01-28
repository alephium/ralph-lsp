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
      case Some(Node(assignment: SoftAST.Assignment, _)) if assignment.expressionLeft == identNode.data =>
        Iterator.single(
          SourceLocation.NodeSoft(
            ast = identNode.data.code,
            source = sourceCode
          )
        )

      case Some(Node(assignment: SoftAST.MutableBinding, _)) if assignment.identifier == identNode.data =>
        Iterator.single(
          SourceLocation.NodeSoft(
            ast = identNode.data.code,
            source = sourceCode
          )
        )

      case _ =>
        searchScope(
          identNode = identNode,
          sourceCode = sourceCode
        )
    }

  /**
   * Searches for definitions given the location of the identifier node [[SoftAST.Identifier]]
   * and the [[SourceLocation]] of the identifier.
   *
   * @param identNode  The node representing the identifier being searched.
   * @param sourceCode The body-part and its source code state where this search is executed.
   * @return An iterator over definition search results.
   */
  private def searchScope(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    sourceCode // search within scope
      .body
      .toNode
      .walkDown
      .flatMap {
        case Node(variable: SoftAST.VariableDeclaration, _) =>
          checkVariableDeclaration(
            variableDec = variable,
            identNode = identNode,
            sourceCode = sourceCode
          )

        case _ =>
          Iterator.empty
      }

  /**
   * Checks if the given identifier is defined by the specified variable declaration.
   *
   * @param variableDec The variable declaration to check.
   * @param identNode   The node representing the identifier being searched for.
   * @param sourceCode  The body part containing the variable declaration.
   * @return The [[SourceLocation]] of the [[SoftAST.CodeString]] where the identifier is defined, if found, else [[None]].
   */
  private def checkVariableDeclaration(
      variableDec: SoftAST.VariableDeclaration,
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Option[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    variableDec.assignment.expressionLeft match {
      case id: SoftAST.Identifier if id.code.text == identNode.data.code.text =>
        Some(
          SourceLocation.NodeSoft(
            ast = id.code,
            source = sourceCode
          )
        )

      case _ =>
        None
    }

}
