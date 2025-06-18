// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object ForParser {

  def parseOrFail[Unknown: P]: P[SoftAST.For] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.For) ~
        TokenParser.isBoundary(Token.OpenParen) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.OpenParen) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.Semicolon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.Semicolon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expression) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.CloseParen) ~
        SpaceParser.parseOrFail.? ~
        BlockParser.parseOrFail.? ~
        Index
    } map {
      case (from,
            forToken,
            postForSpace,
            openParen,
            postOpenParenSpace,
            expression1,
            postExpression1Space,
            postExpression1Semicolon,
            postExpression1SemicolonSpace,
            expression2,
            postExpression2Space,
            postExpression2Semicolon,
            postExpression2SemicolonSpace,
            expression3,
            postExpression3Space,
            closeParen,
            postCloseParenSpace,
            block,
            to
          ) =>
        SoftAST.For(
          index = range(from, to),
          forToken = forToken,
          postForSpace = postForSpace,
          openParen = openParen,
          postOpenParenSpace = postOpenParenSpace,
          expression1 = expression1,
          postExpression1Space = postExpression1Space,
          postExpression1Semicolon = postExpression1Semicolon,
          postExpression1SemicolonSpace = postExpression1SemicolonSpace,
          expression2 = expression2,
          postExpression2Space = postExpression2Space,
          postExpression2Semicolon = postExpression2Semicolon,
          postExpression2SemicolonSpace = postExpression2SemicolonSpace,
          expression3 = expression3,
          postExpression3Space = postExpression3Space,
          closeParen = closeParen,
          postCloseParenSpace = postCloseParenSpace,
          block = block
        )
    }

  private def expression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      AssignmentParser.parseOrFail |
        InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        VariableDeclarationParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        IfElseParser.parseOrFail |
        ElseParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        AnnotationParser.parseOrFail |
        ParameterParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        StringLiteralParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
