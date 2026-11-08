// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object InfixTokenParser {

  /**
   * Parses all tokens of type [[Token.InfixOperator]] and also their comments.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.TokenDocumented[Token.InfixOperator]] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        Index ~
        infixToken ~
        Index
    } map {
      case (commentFrom, documentation, tokenFrom, token, to) =>
        SoftAST.TokenDocumented(
          index = range(commentFrom, to),
          documentation = documentation,
          code = SoftAST.CodeToken(
            index = range(tokenFrom, to),
            token = token
          )
        )
    }

  private def infixToken[Unknown: P]: P[Token.InfixOperator] =
    TokenParsers.parse(Token.infix.toSeq, false)

}
