// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object WhileParser {

  def parseOrFail[Unknown: P]: P[SoftAST.While] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.While) ~
        TokenParser.isBoundary(Token.OpenParen) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.OpenParen) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.CloseParen) ~
        SpaceParser.parseOrFail.? ~
        BlockParser.parseOrFail.? ~
        Index
    } map {
      case (from, whileToken, postWhileSpace, openParen, postOpenParenSpace, expression, postExpressionSpace, closeParen, postCloseParenSpace, block, to) =>
        SoftAST.While(
          index = range(from, to),
          whileToken = whileToken,
          postWhileSpace = postWhileSpace,
          openParen = openParen,
          postOpenParenSpace = postOpenParenSpace,
          expression = expression,
          postExpressionSpace = postExpressionSpace,
          closeParen = closeParen,
          postCloseParenSpace = postCloseParenSpace,
          block = block
        )
    }

  private def expression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        IfElseParser.parseOrFail |
        ElseParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        AnnotationParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        ArrayAccessParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
