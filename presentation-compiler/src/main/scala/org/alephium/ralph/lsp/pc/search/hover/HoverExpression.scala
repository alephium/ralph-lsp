// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.Ast
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.ClientLogger

private case object HoverExpression {

  /**
   * Builds the hover content for a given expression.
   *
   * @param expression The expression to build the hover content for.
   * @param sourceCode The source code location where this request was executed.
   * @param workspace  The workspace state where the source code is located.
   * @return An optional hover content if the expression is supported.
   */
  def apply(
      expression: SoftAST.ExpressionAST,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Option[SourceLocation.Hover] =
    buildExpressionContent(expression, sourceCode, workspace).map {
      case (content, index) =>
        SourceLocation.Hover(
          content = content,
          contentPosition = index,
          parsed = sourceCode
        )
    }

  /**
   * Builds the hover content for a given AST expression.
   *
   * This function checks the type of the expression and builds the content accordingly.
   *
   * @param expression The expression to build the hover content for.
   * @return An optional tuple containing the content and its source index.
   */
  private def buildExpressionContent(
      expression: SoftAST.ExpressionAST,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Option[(String, SourceIndex)] =
    expression match {
      case assignment: SoftAST.Assignment => buildAssignmentContent(assignment, sourceCode, workspace)
      case _                              => None
    }

  /**
   * Builds the hover content for a given assignment expression.
   *
   * This function retrieves the type of the assignment's left-hand side and constructs
   * the content to be displayed in the hover.
   *
   * If the type is not found, it simply returns the assignment code.
   *
   * @param assignment The assignment expression to build the content for.
   * @param sourceCode The source code state containing the parsed code.
   * @param workspace  The workspace state where the source code is located.
   * @return An optional tuple containing the content and its source index.
   */
  private def buildAssignmentContent(
      assignment: SoftAST.Assignment,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Option[(String, SourceIndex)] =
    findAssignmentType(assignment, sourceCode, workspace) match {
      case Some(typeId) =>
        // TODO: We need to adapt the index
        Some((s"${assignment.expressionLeft.toCode()}: ${typeId.name}", assignment.index))
      case None =>
        // If the type is not found, just return the assignment code.
        // TODO: Do we want this or not?
        Some((assignment.toCode(), assignment.index))
    }

  /**
   * Finds the type of the assignment expression.
   *
   * This function searches for the type definition of the assignment's left-hand side
   * and returns its name if found.
   *
   * @param assignment The assignment expression to find the type for.
   * @param sourceCode The source code state containing the parsed code.
   * @param workspace  The workspace state where the source code is located.
   * @return An optional string representing the type name, if found.
   */
  private def findAssignmentType(
      assignment: SoftAST.Assignment,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Option[Ast.TypeId] =
    CodeProvider
      .goToTypeDef
      .search(
        linePosition = assignment.index.toLineRange(sourceCode.code).from,
        fileURI = sourceCode.fileURI,
        workspace = workspace,
        searchSettings = ()
      )
      .flatMap(_.toOption.flatMap {
        typeDef =>
          typeDef.nextOption().map {
            typeDef =>
              typeDef.ast
          }
      })

}
