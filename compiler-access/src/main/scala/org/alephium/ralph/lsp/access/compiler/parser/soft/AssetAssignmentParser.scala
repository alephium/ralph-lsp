// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object AssetAssignmentParser {

  def parseOrFail[Unknown: P]: P[SoftAST.AssetAssignment] =
    P {
      Index ~
        ExpressionParser.parseSubset(leftExpression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Colon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~ // On the right, parse anything other than type-assignment
        Index
    } map {
      case (from, left, postIdentifierSpace, equalToken, postEqualSpace, right, to) =>
        SoftAST.AssetAssignment(
          index = range(from, to),
          expressionLeft = left,
          preColonSpace = postIdentifierSpace,
          colon = equalToken,
          postColonSpace = postEqualSpace,
          expressionRight = right
        )
    }

  private def leftExpression[Unknown: P] =
    P {
      MethodCallParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        StructConstructorParser.parseOrFail |
        ArrayParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        AlphParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        ArrayAccessParser.parseOrFail |
        IdentifierParser.parseOrFail |
        UnresolvedParser.parseOrFailSpaceDelimited(Token.Colon)
    }

}
