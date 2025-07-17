// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object ArrowAssignmentParser {

  /**
   * Syntax:
   * {{{
   *  Identifier -> Expression
   * }}}
   */
  def parseOrFail[Unknown: P]: P[SoftAST.ArrowAssignment] =
    P {
      Index ~
        ExpressionParser.parseSubset(leftExpression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.ForwardArrow) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, leftExpression, preArrowSpace, forwardArrow, preRightExpressionSpace, rightExpression, to) =>
        SoftAST.ArrowAssignment(
          index = range(from, to),
          leftExpression = leftExpression,
          preArrowSpace = preArrowSpace,
          forwardArrow = forwardArrow,
          preRightExpressionSpace = preRightExpressionSpace,
          rightExpression = rightExpression
        )
    }

  private def leftExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      IdentifierParser.parseOrFail |
        UnresolvedParser.parseOrFailSpaceDelimited(Token.ForwardArrow)
    }

}
