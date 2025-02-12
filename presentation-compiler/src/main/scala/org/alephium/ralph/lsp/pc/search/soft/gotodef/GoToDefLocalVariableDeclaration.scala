package org.alephium.ralph.lsp.pc.search.soft.gotodef

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.gotodef.ScopeWalker
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.Node

import scala.annotation.tailrec

object GoToDefLocalVariableDeclaration {

  /**
   * Searches for local variables and argument definitions given the location of the identifier node [[SoftAST.Identifier]]
   * and the [[SourceLocation]] of the identifier.
   *
   * Steps:
   *  - First, checks if the current [[SoftAST.Identifier]] itself belongs to a definition.
   *    These are self-jump-definitions.
   *  - Second, executes search for all nodes within the scope, local to the current block of code.
   *
   * @param identNode  The node representing the identifier being searched.
   * @param sourceCode The body-part and its source code state where this search is executed.
   * @return An iterator over definition search results.
   */
  def apply(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.GoToDefSoft] =
    identNode.parent match {
      case Some(Node(assignment: SoftAST.VariableDeclaration, _)) if assignment.assignment.expressionLeft == identNode.data =>
        Iterator.single(
          SourceLocation.NodeSoft(
            ast = identNode.data.code,
            source = sourceCode
          )
        )

      case Some(node @ Node(assignment: SoftAST.Assignment, _)) if assignment.expressionLeft == identNode.data =>
        node.parent match {
          // If it's an assignment, it must also be a variable declaration for the current node to be a self.
          case Some(Node(_: SoftAST.VariableDeclaration, _)) =>
            Iterator.single(
              SourceLocation.NodeSoft(
                ast = identNode.data.code,
                source = sourceCode
              )
            )

          case _ =>
            // invoke full scope search.
            search(
              identNode = identNode,
              sourceCode = sourceCode
            )
        }

      case Some(Node(assignment: SoftAST.TypeAssignment, _)) if assignment.expressionLeft == identNode.data =>
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
        search(
          identNode = identNode,
          sourceCode = sourceCode
        )
    }

  /**
   * Searches for definitions with the local scope of the current block.
   *
   * Within the [[ScopeWalker]] block, define all expressions where the local variables and arguments could be present:
   *
   * A local variable can only be defined as a:
   *  - Variable-declaration `let variable = 1` ([[SoftAST.VariableDeclaration]]).
   *  - Type-assignment `variable: U256` ([[SoftAST.TypeAssignment]]).
   *  - Mutable-binding `mut variable` ([[SoftAST.MutableBinding]]).
   *
   * The goal here is to search within the above ASTs.
   *
   * @param identNode  The node representing the identifier being searched.
   * @param sourceCode The body-part and its source code state where this search is executed.
   * @return An iterator over definition search results.
   */
  private def search(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    ScopeWalker
      .walk(
        from = sourceCode.body.toNode,
        anchor = identNode.data
      ) {
        case Node(variable: SoftAST.VariableDeclaration, _) =>
          expandAndSearchExpression(
            expression = variable,
            identNode = identNode,
            sourceCode = sourceCode
          )

        case Node(assignment: SoftAST.TypeAssignment, _) =>
          expandAndSearchExpression(
            expression = assignment,
            identNode = identNode,
            sourceCode = sourceCode
          )

        case Node(binding: SoftAST.MutableBinding, _) =>
          expandAndSearchExpression(
            expression = binding,
            identNode = identNode,
            sourceCode = sourceCode
          )
      }
      .iterator
      .flatten

  /**
   * Given a collection of expressions, expands each expression and searches within it for all possible definitions.
   *
   * @param expressions The expressions to expand and search.
   * @param identNode   The node representing the identifier being searched.
   * @param sourceCode  The source code state where this search is executed.
   * @return An iterator over the locations of the definitions.
   */
  private def expandAndSearchExpressions(
      expressions: Iterable[SoftAST.ExpressionAST],
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    expressions
      .iterator
      .flatMap {
        expression =>
          expandAndSearchExpression(
            expression = expression,
            identNode = identNode,
            sourceCode = sourceCode
          )
      }

  /**
   * Given an expression, expands the expression and searches within it for all possible definitions.
   *
   * @param expression The expression to expand and search.
   * @param identNode  The node representing the identifier being searched.
   * @param sourceCode The source code state where this search is executed.
   * @return An iterator over the locations of the definitions.
   */
  @tailrec
  private def expandAndSearchExpression(
      expression: SoftAST.ExpressionAST,
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    expression match {
      case ast: SoftAST.VariableDeclaration =>
        // expand variable declaration and search within the assignment
        expandAndSearchExpression(
          expression = ast.assignment,
          identNode = identNode,
          sourceCode = sourceCode
        )

      case ast: SoftAST.TypeAssignment =>
        // expand type assigment and search within the left expression
        expandAndSearchExpression(
          expression = ast.expressionLeft,
          identNode = identNode,
          sourceCode = sourceCode
        )

      case group: SoftAST.Group[_, _] =>
        // Expand the group and search the expressions within
        expandAndSearchExpressions(
          expressions = group.expressions,
          identNode = identNode,
          sourceCode = sourceCode
        )

      case SoftAST.Assignment(_, left, _, _, _, _) =>
        // Expand the expression within this assignment and search within
        expandAndSearchExpression(
          expression = left,
          identNode = identNode,
          sourceCode = sourceCode
        )

      case binding: SoftAST.MutableBinding =>
        // Search the identifier
        binding.identifier match {
          case identifier: SoftAST.Identifier =>
            expandAndSearchExpression(
              expression = identifier,
              identNode = identNode,
              sourceCode = sourceCode
            )

          case _: SoftAST.IdentifierExpected =>
            // identifier not provided
            Iterator.empty
        }

      case SoftAST.Identifier(_, _, code) if code.text == identNode.data.code.text =>
        // Check if the identifier matches the text in the selected `identNode`.
        Iterator.single(
          SourceLocation.NodeSoft(
            ast = code,
            source = sourceCode
          )
        )

      case _ =>
        Iterator.empty
    }

}
