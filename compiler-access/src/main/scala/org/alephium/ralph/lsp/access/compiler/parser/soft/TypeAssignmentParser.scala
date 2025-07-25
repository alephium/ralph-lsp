// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TypeAssignmentParser {

  def parse[Unknown: P]: P[SoftAST.TypeAssignment] =
    P {
      Index ~
        AnnotationParser.parseOrFail.rep ~
        ExpressionParser.parseSubset(leftExpression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.Colon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(rightExpression) ~
        Index
    } flatMap {
      case (_, _, _: SoftAST.ExpressionExpected, _, _: SoftAST.TokenExpected[_], _, _: SoftAST.ExpressionAST, _) =>
        // If all parsers fail, fail this parser call.
        Fail(s"Unable to parse ${classOf[SoftAST.TypeAssignment].getSimpleName}")

      case (from, annotations, left, postIdentifierSpace, colon, postEqualSpace, right, to) =>
        val ast =
          SoftAST.TypeAssignment(
            index = range(from, to),
            annotations = annotations,
            expressionLeft = left,
            preColonSpace = postIdentifierSpace,
            colon = colon,
            postColonSpace = postEqualSpace,
            expressionRight = right
          )

        Pass(ast)
    }

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
      TupleParser.parseOrFail(assertNonEmpty = false) |
        ArrayParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
