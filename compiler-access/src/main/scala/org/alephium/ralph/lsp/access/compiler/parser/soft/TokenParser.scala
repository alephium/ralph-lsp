// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.util.ParserUtil

private object TokenParser {

  def parse[Unknown: P, T <: Token](
      required: Boolean,
      token: T): P[SoftAST.TokenDocExpectedAST[T]] =
    if (required)
      parse(token)
    else
      parseOrFail(token)

  def parse[Unknown: P, T <: Token](token: T): P[SoftAST.TokenDocExpectedAST[T]] =
    P(Index ~ parseOrFail(token).?) map {
      case (_, Some(token)) =>
        token

      case (from, None) =>
        SoftAST.TokenExpected(point(from), token)
    }

  def parseOrFail[Unknown: P, T <: Token](token: T): P[SoftAST.TokenDocumented[T]] =
    P(Index ~ CommentParser.parseOrFail.? ~ CodeParser.parseOrFail(token) ~ Index) map {
      case (from, documentation, token, to) =>
        SoftAST.TokenDocumented(
          index = range(from, to),
          documentation = documentation,
          code = token
        )
    }

  def parseOrFailUndocumented[Unknown: P, T <: Token](token: T): P[SoftAST.TokenUndocumented[T]] =
    P(CodeParser.parseOrFail(token)) map (SoftAST.TokenUndocumented(_))

  /**
   * Parses all reserved tokens defined in [[Token.reserved]] and returns the first match.
   */
  def Reserved[Unknown: P]: P[Token.Reserved] =
    ParserUtil.orTokenCombinator(Token.reserved.iterator)

  /**
   * Parses all tokens of type [[Token.InfixOperator]] and also their comments.
   */
  def InfixOperatorOrFail[Unknown: P]: P[SoftAST.TokenDocumented[Token.InfixOperator]] = {
    val infixOps =
      ParserUtil.orCombinator(
        items = Token.infix.iterator.filter(_ != Token.ForwardSlash), // remove forward-slash
        parser = TokenParser.parseOrFail(_: Token.InfixOperator)
      )

    // Forward-slash followed by another forward-slash is not an Operator.
    // `//` is reserved as a comment prefix.
    def forwardSlashOperator =
      P(TokenParser.parseOrFail(Token.ForwardSlash) ~ !Token.ForwardSlash.lexeme)

    infixOps | forwardSlashOperator
  }

}
