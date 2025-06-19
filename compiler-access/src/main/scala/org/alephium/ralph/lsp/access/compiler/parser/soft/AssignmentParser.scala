// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

/**
 * Syntax:
 *
 * {{{
 *    let mut variable = 1
 *        ↑______________↑
 * }}}
 */
private object AssignmentParser {

  /**
   * Parses a required assignment and reports errors if it is missing.
   */
  def parse[Unknown: P]: P[SoftAST.Assignment] =
    P {
      Index ~
        ExpressionParser.parseSubset(leftExpression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.Equal) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(rightExpression) ~
        Index
    } map {
      case (from, left, postIdentifierSpace, equalToken, postEqualSpace, right, to) =>
        SoftAST.Assignment(
          index = range(from, to),
          expressionLeft = left,
          postIdentifierSpace = postIdentifierSpace,
          equalToken = equalToken,
          postEqualSpace = postEqualSpace,
          expressionRight = right
        )
    }

  /**
   * Parses an assignment, fails if parsing is not possible.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Assignment] =
    P {
      Index ~
        leftExpression ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Equal) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(rightExpression) ~
        Index
    } map {
      case (from, identifier, postIdentifierSpace, equalToken, postEqualSpace, expression, to) =>
        SoftAST.Assignment(
          index = range(from, to),
          expressionLeft = identifier,
          postIdentifierSpace = postIdentifierSpace,
          equalToken = equalToken,
          postEqualSpace = postEqualSpace,
          expressionRight = expression
        )
    }

  /** TODO: Review this function - Most of these parsers can be removed. */
  private def leftExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      MethodCallParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        TupleParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

  private def rightExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        BlockParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        IfElseParser.parseOrFail |
        ElseParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        AnnotationParser.parseOrFail |
        TupleParser.parseOrFail |
        ArrayParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
