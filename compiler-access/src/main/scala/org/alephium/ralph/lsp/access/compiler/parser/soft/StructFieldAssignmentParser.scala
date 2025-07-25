// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object StructFieldAssignmentParser {

  def parse[Unknown: P]: P[SoftAST.StructFieldAssignment] =
    P {
      Index ~
        ExpressionParser.parseSubset(leftExpression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.Colon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(rightExpression) ~
        Index
    } flatMap {
      case (_, _: SoftAST.ExpressionExpected, _, _: SoftAST.TokenExpected[_], _, _: SoftAST.ExpressionAST, _) =>
        // If all parsers fail, fail this parser call.
        Fail(s"Unable to parse ${classOf[SoftAST.StructFieldAssignment].getSimpleName}")

      case (from, left, postIdentifierSpace, equalToken, postEqualSpace, right, to) =>
        val ast =
          SoftAST.StructFieldAssignment(
            index = range(from, to),
            expressionLeft = left,
            preColonSpace = postIdentifierSpace,
            colon = equalToken,
            preExpressionSpace = postEqualSpace,
            expressionRight = right
          )

        Pass(ast)
    }

  private def leftExpression[Unknown: P] =
    P {
      MutableBindingParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

  private def rightExpression[Unknown: P] =
    P {
      InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        IfElseParser.parseOrFail |
        ElseParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        StructConstructorParser.parseOrFail |
        AnnotationParser.parseOrFail |
        TupleParser.parseOrFail(assertNonEmpty = false) |
        ArrayParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        AlphParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        ArrayAccessParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
