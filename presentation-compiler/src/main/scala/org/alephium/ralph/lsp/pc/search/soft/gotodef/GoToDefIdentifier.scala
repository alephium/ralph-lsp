package org.alephium.ralph.lsp.pc.search.soft.gotodef

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.gotodef.ScopeWalker
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.Node

import scala.annotation.tailrec

private object GoToDefIdentifier {

  /**
   * Searches definitions given the location of the identifier node [[SoftAST.Identifier]]
   * and the [[SourceLocation]] of the identifier.
   *
   * Steps:
   *  - First, checks if the current [[SoftAST.Identifier]] itself belongs to a definition.
   *    These are self-jump-definitions.
   *  - Second, executes definition search within the workspace.
   *
   * @param identNode  The node representing the identifier being searched.
   * @param sourceCode The block-part and its source code state where this search is executed.
   * @return An iterator over definition search results.
   */
  def apply(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.GoToDefSoft] =
    identNode.parent match {
      case Some(Node(_: SoftAST.ReferenceCall | _: SoftAST.DotCall, _)) =>
        // TODO: Handle function and dot calls
        Iterator.empty

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
   * @param sourceCode The block-part and its source code state where this search is executed.
   * @return An iterator over definition search results.
   */
  private def search(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    ScopeWalker
      .walk(
        from = sourceCode.part.toNode,
        anchor = identNode.data.index
      ) {
        case Node(variable: SoftAST.VariableDeclaration, _) =>
          searchExpression(
            expression = variable,
            target = identNode.data,
            sourceCode = sourceCode
          )

        case Node(assignment: SoftAST.TypeAssignment, _) =>
          searchExpression(
            expression = assignment,
            target = identNode.data,
            sourceCode = sourceCode
          )

        case Node(binding: SoftAST.MutableBinding, _) =>
          searchExpression(
            expression = binding,
            target = identNode.data,
            sourceCode = sourceCode
          )
      }
      .iterator

  /**
   * Given a collection of expressions, expands each expression and searches within it for all possible definitions.
   *
   * @param expressions The expressions to expand and search.
   * @param target      The identifier being searched.
   * @param sourceCode  The source code state where this search is executed.
   * @return An iterator over the locations of the definitions.
   */
  private def searchExpressions(
      expressions: Iterable[SoftAST.ExpressionAST],
      target: SoftAST.Identifier,
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    expressions
      .iterator
      .flatMap {
        expression =>
          searchExpression(
            expression = expression,
            target = target,
            sourceCode = sourceCode
          )
      }

  /**
   * Expands the given identifier and matches its name against a target.
   *
   * @param identifier The identifier to expand and match.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where this search is executed.
   * @return An iterator over the locations of the definitions.
   */
  private def searchIdentifier(
      identifier: SoftAST.IdentifierAST,
      target: SoftAST.Identifier,
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    identifier match {
      case identifier: SoftAST.Identifier =>
        searchExpression(
          expression = identifier,
          target = target,
          sourceCode = sourceCode
        )

      case _: SoftAST.IdentifierExpected =>
        Iterator.empty
    }

  /**
   * Given an expression, expands the expression and searches within it for all possible definitions.
   *
   * @param expression The expression to expand and search.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where this search is executed.
   * @return An iterator over the locations of the definitions.
   */
  @tailrec
  private def searchExpression(
      expression: SoftAST.ExpressionAST,
      target: SoftAST.Identifier,
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    expression match {
      case ast: SoftAST.VariableDeclaration =>
        // expand variable declaration and search within the assignment
        searchExpression(
          expression = ast.assignment,
          target = target,
          sourceCode = sourceCode
        )

      case ast: SoftAST.TypeAssignment =>
        // expand type assigment and search within the left expression
        searchExpression(
          expression = ast.expressionLeft,
          target = target,
          sourceCode = sourceCode
        )

      case group: SoftAST.Group[_, _] =>
        // Expand the group and search the expressions within
        searchExpressions(
          expressions = group.expressions,
          target = target,
          sourceCode = sourceCode
        )

      case assignment: SoftAST.Assignment =>
        // Expand the expression within this assignment and search within
        searchExpression(
          expression = assignment.expressionLeft,
          target = target,
          sourceCode = sourceCode
        )

      case binding: SoftAST.MutableBinding =>
        // Search the identifier
        searchIdentifier(
          identifier = binding.identifier,
          target = target,
          sourceCode = sourceCode
        )

      case identifier: SoftAST.Identifier if identifier.code.text == target.code.text =>
        // Check if the identifier matches the text in the selected `identNode`.
        Iterator.single(
          SourceLocation.NodeSoft(
            ast = identifier.code,
            source = sourceCode
          )
        )

      case _ =>
        Iterator.empty
    }

}
