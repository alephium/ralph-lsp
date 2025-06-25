// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.{Ast, SourceIndex}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast._
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node

private case object HoverExpression extends StrictImplicitLogging {

  /**
   * Executes hover search for a given expression in the source code.
   *
   * This method checks if the expression is an assignment or a variable declaration,
   * and retrieves the appropriate hover content based on the expression type.
   *
   * @param expression The expression to search for hover content.
   * @param sourceCode The source code state containing the parsed code.
   * @param workspace  The workspace state where the source code is located.
   * @return An optional hover content for the expression, if applicable.
   */
  def apply(
      expression: SoftAST.ExpressionAST,
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Option[SourceLocation.Hover] =
    expression match {
      case assignment: SoftAST.Assignment =>
        hoverAssignment(assignment, sourceCode, workspace)

      case variableDeclaration: SoftAST.VariableDeclaration =>
        hoverVariableDeclaration(variableDeclaration, sourceCode, workspace).map {
          content =>
            SourceLocation.Hover(
              content = content,
              code = sourceCode
            )
        }

      case other =>
        logger.error(s"Hover not implemented for expression '${other.getClass.getSimpleName}' at source index '${other.index}'")
        None
    }

  /**
   * Retrieves hover content for an assignment expression.
   *
   * If the assignment is a variable declaration, it resolves the type and prepares the hover content.
   * If the assignment is a declaration, it uses the declaration's hover content.
   *
   * @param assignment The assignment expression to process.
   * @param sourceCode The source code state.
   * @param workspace  The workspace state.
   * @return An optional hover content for the assignment.
   */
  private def hoverAssignment(
      assignment: SoftAST.Assignment,
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Option[SourceLocation.Hover] =
    // Assignment can be a `let` or a `const`.
    assignment.toNode.parent match {
      case Some(Node(variableDeclaration: SoftAST.VariableDeclaration, _)) =>
        HoverExpression(variableDeclaration, sourceCode, workspace)

      case Some(Node(declaration: SoftAST.DeclarationAST, _)) =>
        HoverDeclaration(declaration, sourceCode)

      case _ =>
        // An `Assignment` without a parent should not exist.
        None
    }

  /**
   * Retrieves hover content for a variable declaration.
   *
   * Attempts to resolve the type of the variable's assignment and creates
   * the hover content accordingly. Falls back to the declaration code if no type is found.
   *
   * @param variableDeclaration The variable declaration to process.
   * @param sourceCode          The source code state.
   * @param workspace           The workspace state.
   * @return An optional variable declaration for hover display.
   */
  private def hoverVariableDeclaration(
      variableDeclaration: SoftAST.VariableDeclaration,
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Option[SoftAST.VariableDeclaration] =
    findAssignmentType(variableDeclaration.assignment, sourceCode, workspace) match {
      case Some(typeId) =>
        Some(hoverVariableDeclarationWithType(variableDeclaration, typeId))
      case None =>
        // If the type is not found, just return the assignment code.
        // TODO: Consider if this fallback is desirable, especially for long expressions.
        Some(variableDeclaration)
    }

  /**
   * Returns a variable declaration with type information suitable for hover display.
   *
   * Modifies the variable declaration to present its name and type, omitting the value assignment.
   *
   * @param variableDeclaration The variable declaration to process.
   * @param typeId The type identifier to display.
   * @return A modified variable declaration with type information for hover display.
   */
  private def hoverVariableDeclarationWithType(
      variableDeclaration: SoftAST.VariableDeclaration,
      typeId: Ast.TypeId): SoftAST.VariableDeclaration = {

    val varDec = variableDeclaration.deepCopy(SourceIndex.empty)

    // Create a type assignment node showing the variable and its type.
    val typeAssignment =
      SoftAST.TypeAssignment(
        index = SourceIndex.empty,
        annotations = Seq.empty,
        expressionLeft = varDec.assignment.expressionLeft,
        preColonSpace = None,
        colon = SoftAST.TokenDocumented(SourceIndex.empty, None, SoftAST.CodeToken(SourceIndex.empty, Token.Colon)),
        postColonSpace = varDec.assignment.postIdentifierSpace,
        expressionRight = SoftAST.Identifier(SourceIndex.empty, None, SoftAST.CodeString(SourceIndex.empty, typeId.name))
      )

    varDec.copy(
      assignment = varDec
        .assignment
        .copy(
          // Replace the left-hand side with the type assignment for display.
          expressionLeft = typeAssignment,
          postIdentifierSpace = None,
          // Remove the equal token (=).
          equalToken = SoftAST.TokenExpected(SourceIndex.empty, Token.Equal),
          postEqualSpace = None,
          // Remove the assigned value.
          expressionRight = SoftAST.ExpressionExpected(SourceIndex.empty)
        ),
      // TODO: Add documentation if needed
      let = varDec.let.copy(documentation = None)
    )
  }

  /**
   * Attempts to resolve the type of an assignment expression.
   *
   * Searches for the type definition associated with the assignment's left side,
   * returning the TypeId if found.
   *
   * @param assignment The assignment to resolve the type for.
   * @param sourceCode The source code state.
   * @param workspace  The workspace state.
   * @return An optional type identifier for the assignment, if resolved.
   */
  private def findAssignmentType(
      assignment: SoftAST.Assignment,
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Option[Ast.TypeId] = {
    // Find all the type definitions
    val typeDefs = CodeProvider
      .goToTypeDef
      .search(
        linePosition = assignment.index.toLineRange(sourceCode.parsed.code).from,
        fileURI = sourceCode.parsed.fileURI,
        workspace = workspace,
        searchSettings = ()
      )

    typeDefs match {
      case Some(Right(typeDefs)) =>
        // TODO: What should we do if multiple type definitions are found?
        typeDefs.nextOption().map(_.ast)

      case Some(Left(error)) =>
        logger.error(s"Error searching type-definition for '${assignment.toCode()}'. Reason: ${error.message}. FileURI: ${sourceCode.parsed.fileURI}")
        None

      case None =>
        logger.info(s"No type-definitions found for '${assignment.toCode()}'. FileURI: ${sourceCode.parsed.fileURI}")
        None
    }
  }

}
