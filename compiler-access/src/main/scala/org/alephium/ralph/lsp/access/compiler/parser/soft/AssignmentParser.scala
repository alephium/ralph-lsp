package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

/**
 * Syntax:
 *
 * {{{
 *    let mut variable = 1
 *        ↑______________↑
 * }}}
 */
private case object AssignmentParser {

  /**
   * Parses a required assignment and reports errors if it is missing.
   */
  def parse[Unknown: P]: P[SoftAST.Assignment] =
    P {
      Index ~
        leftExpression.? ~
        SpaceParser.parseOrFail.? ~
        Index ~
        TokenParser.parseOrFail(Token.Equal).? ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, left, postIdentifierSpace, equalIndex, equalToken, postEqualSpace, right, to) =>
        SoftAST.Assignment(
          index = range(from, to),
          expressionLeft = left getOrElse SoftAST.ExpressionExpected(point(from)),
          postIdentifierSpace = postIdentifierSpace,
          equalToken = equalToken getOrElse SoftAST.TokenExpected(point(equalIndex), Token.Equal),
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
        ExpressionParser.parse ~
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

  private def leftExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      MethodCallParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        ParameterParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
