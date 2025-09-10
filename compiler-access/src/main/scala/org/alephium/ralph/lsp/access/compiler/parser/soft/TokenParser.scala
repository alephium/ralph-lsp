// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        CodeParser.parseOrFail(token) ~
        Index
    } map {
      case (from, documentation, token, to) =>
        SoftAST.TokenDocumented(
          index = range(from, to),
          documentation = documentation,
          code = token
        )
    }

  /**
   * Parses the first successful token from the given iterator of tokens, failing if none succeed.
   *
   * @param prefixCheck If `true`, performs the prefix check before parsing each token.
   * @param tokens      The tokens to parse.
   */
  def parseOrFailOneOf[Unknown: P, T <: Token](
      prefixCheck: Boolean,
      tokens: Iterator[T]): P[SoftAST.TokenDocumented[T]] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        Index ~
        ParserUtil.orTokenCombinator(prefixCheck = prefixCheck, tokens = tokens) ~
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

  /**
   * Reads characters until at least one of the input tokens is matched.
   *
   * If none of the tokens are found, the parser fails.
   *
   * @param tokens the token to check for.
   */
  def WhileNotOrFail[Unknown: P](tokens: Seq[Token]): P[Unit] =
    P((!ParserUtil.orTokenCombinator(prefixCheck = true, tokens = tokens.iterator) ~ AnyChar).rep(1))

  def WhileInOrFail[Unknown: P](tokens: Seq[Token]): P[Unit] =
    P(ParserUtil.orTokenCombinator(prefixCheck = true, tokens = tokens.iterator).rep(1))

  /**
   * Checks if the next character breaks (token boundary) the previously parsed token.
   */
  def isBoundary[Unknown: P](): P[Unit] =
    P(&(CharPred(!_.isLetterOrDigit) | End))

}
