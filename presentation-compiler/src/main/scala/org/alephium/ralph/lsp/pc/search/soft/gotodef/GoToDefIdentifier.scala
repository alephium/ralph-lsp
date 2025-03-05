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

      case Some(Node(function: SoftAST.FunctionSignature, _)) if function.fnName == identNode.data =>
        Iterator.single(
          SourceLocation.NodeSoft(
            ast = identNode.data.code,
            source = sourceCode
          )
        )

      case Some(Node(template: SoftAST.Template, _)) if template.identifier == identNode.data =>
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
   * Searches for occurrences of the given identifier node within the source code.
   *
   * @param identNode  The identifier node to search for.
   * @param sourceCode The source code state where the search is performed.
   * @return return An iterator over the locations of the definitions.
   */
  private def search(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    searchLocal(
      from = sourceCode.part.toNode,
      target = identNode,
      sourceCode = sourceCode
    ).iterator

  /**
   * Expands and searches for all possible local definitions starting from the given position.
   *
   * @param from       The position from where the search should begin.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where the `from` node is located.
   * @return An iterator over the locations of the definitions.
   */
  private def searchLocal(
      from: Node[SoftAST, SoftAST],
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterable[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    ScopeWalker
      .walk(
        from = from,
        anchor = target.data.index
      ) {
        case Node(variable: SoftAST.VariableDeclaration, _) =>
          searchExpression(
            expression = variable,
            target = target,
            sourceCode = sourceCode
          )

        case Node(assignment: SoftAST.TypeAssignment, _) =>
          searchExpression(
            expression = assignment,
            target = target,
            sourceCode = sourceCode
          )

        case Node(binding: SoftAST.MutableBinding, _) =>
          searchExpression(
            expression = binding,
            target = target,
            sourceCode = sourceCode
          )

        case Node(function: SoftAST.Function, _) =>
          searchFunction(
            function = function,
            target = target,
            sourceCode = sourceCode
          )

        case Node(template: SoftAST.Template, _) =>
          searchTemplate(
            template = template,
            target = target,
            sourceCode = sourceCode
          )
      }

  /**
   * Given a function, expands and searches within it for all possible definitions.
   *
   * @param function   The function to expand and search.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where the function belongs.
   * @return An iterator over the locations of the definitions.
   */
  private def searchFunction(
      function: SoftAST.Function,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft) = {
    // Check if searched identifier is a reference call
    val isReferenceCall = target.isReferenceCall()

    // If the identifier belongs the function's block, search the parameters and the block.
    val blockMatches =
      function.block match {
        case Some(block) if block.contains(target) =>
          // Search the parameters
          val paramMatches =
            if (isReferenceCall)
              Iterator.empty
            else
              searchExpression(
                expression = function.signature.params,
                target = target,
                sourceCode = sourceCode
              )

          // search the block
          val blockMatches =
            searchBlock(
              block = block,
              target = target,
              sourceCode = sourceCode
            )

          paramMatches ++ blockMatches

        case _ =>
          Iterator.empty
      }

    // Check if the name matches the identifier.
    val nameMatches =
      if (isReferenceCall)
        searchIdentifier(
          identifier = function.signature.fnName,
          target = target,
          sourceCode = sourceCode
        )
      else
        Iterator.empty

    nameMatches ++ blockMatches
  }

  /**
   * Given a template, expands and searches within it for all possible definitions.
   *
   * @param template   The template to expand and search.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where the function belongs.
   * @return An iterator over the locations of the definitions.
   */
  private def searchTemplate(
      template: SoftAST.Template,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft) = {
    val blockMatches =
      template.block match {
        case Some(block) if block.contains(target) || template.inheritance.exists(_.contains(target)) =>
          // Search the parameters
          val paramMatches =
            template.params match {
              case Some(params) =>
                searchExpression(
                  expression = params,
                  target = target,
                  sourceCode = sourceCode
                )

              case None =>
                Iterator.empty
            }

          // search the block
          val blockMatches =
            searchBlock(
              block = block,
              target = target,
              sourceCode = sourceCode
            )

          paramMatches ++ blockMatches

        case _ =>
          Iterator.empty
      }

    // Check if the name matches the identifier.
    val nameMatches =
      searchIdentifier(
        identifier = template.identifier,
        target = target,
        sourceCode = sourceCode
      )

    nameMatches ++ blockMatches
  }

  /**
   * Given a block, expands and searches within it for all possible definitions.
   *
   * @param block      The block to expand and search.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where the function belongs.
   * @return An iterator over the locations of the definitions.
   */
  private def searchBlock(
      block: SoftAST.Block,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterable[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (block contains target)
      searchLocal(
        from = block.toNode,
        target = target,
        sourceCode = sourceCode
      )
    else
      Iterable.empty

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
      target: Node[SoftAST.Identifier, SoftAST],
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
      target: Node[SoftAST.Identifier, SoftAST],
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
      target: Node[SoftAST.Identifier, SoftAST],
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

      case identifier: SoftAST.Identifier if identifier.code.text == target.data.code.text =>
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
