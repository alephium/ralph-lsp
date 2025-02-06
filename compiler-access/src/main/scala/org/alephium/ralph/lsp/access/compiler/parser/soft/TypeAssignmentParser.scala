package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TypeAssignmentParser {

  def parseOrFail[Unknown: P]: P[SoftAST.TypeAssignment] =
    P {
      Index ~
        leftExpression ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Colon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, left, postIdentifierSpace, equalToken, postEqualSpace, right, to) =>
        SoftAST.TypeAssignment(
          index = range(from, to),
          expressionLeft = left,
          preColonSpace = postIdentifierSpace,
          colon = equalToken,
          postColonSpace = postEqualSpace,
          expressionRight = right
        )
    }

  private def leftExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P(Index ~ leftExpressionOrFail.?) map {
      case (_, Some(expression)) =>
        expression

      case (from, None) =>
        SoftAST.ExpressionExpected(point(from))
    }

  private def leftExpressionOrFail[Unknown: P] =
    P {
      MutableBindingParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
