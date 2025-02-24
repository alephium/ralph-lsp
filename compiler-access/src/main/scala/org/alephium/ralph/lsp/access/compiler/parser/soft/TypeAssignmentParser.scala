package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TypeAssignmentParser {

  def parseOrFail[Unknown: P]: P[SoftAST.TypeAssignment] =
    P {
      Index ~
        AnnotationParser.parseOrFail.rep ~
        ExpressionParser.parseSubset(leftExpression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Colon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(rightExpression) ~
        Index
    } map {
      case (from, annotations, left, postIdentifierSpace, equalToken, postEqualSpace, right, to) =>
        SoftAST.TypeAssignment(
          index = range(from, to),
          annotations = annotations,
          expressionLeft = left,
          preColonSpace = postIdentifierSpace,
          colon = equalToken,
          postColonSpace = postEqualSpace,
          expressionRight = right
        )
    }

  private def leftExpression[Unknown: P] =
    P {
      MutableBindingParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

  private def rightExpression[Unknown: P] =
    P {
      ParameterParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
