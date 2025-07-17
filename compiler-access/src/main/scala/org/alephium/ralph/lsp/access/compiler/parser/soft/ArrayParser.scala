// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object ArrayParser {

  def parseOrFail[Unknown: P]: P[SoftAST.ArrayAST] =
    P(sized | inline)

  private def sized[Unknown: P]: P[SoftAST.ArraySized] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.OpenBracket) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Semicolon) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.BlockBracket) ~
        Index
    } map {
      case (from, openBracket, preTypeSpace, tpe, preSemiColonSpace, semiColon, preSizeSpace, size, preCloseBracketSpace, closeBracket, to) =>
        SoftAST.ArraySized(
          index = range(from, to),
          openBracket = openBracket,
          preTypeSpace = preTypeSpace,
          tpe = tpe,
          preSemiColonSpace = preSemiColonSpace,
          semiColon = semiColon,
          preSizeSpace = preSizeSpace,
          size = size,
          preCloseBracketSpace = preCloseBracketSpace,
          closeBracket = closeBracket
        )
    }

  private def inline[Unknown: P]: P[SoftAST.ArrayInline] =
    GroupParser
      .parseOrFail(
        assertNonEmpty = false,
        open = Token.OpenBracket,
        close = Token.BlockBracket,
        delimiter = Token.Comma,
        expressionsParseOrFail = GroupParser.defaultExpressions
      )
      .map(SoftAST.ArrayInline)

}
