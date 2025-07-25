// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.util

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token

object ParserUtil {

  /**
   * Combines all items using the "or" combinator with the given parser.
   *
   * <b>Prerequisite</b>: The `items` iterator must not be empty.
   *
   * @param items  The items to be parsed.
   * @param parser The parser to execute.
   * @tparam Unknown The current fastparse context.
   * @tparam I       The type of input items.
   * @tparam O       The type of output produced by the parser.
   * @return A combined parser all input items using the "or" combinator.
   */
  def orCombinator[Unknown: P, I, O](
      items: Iterator[I],
      parser: I => P[O]): P[O] =
    items.nextOption() match {
      case Some(head) =>
        items.foldLeft(parser(head))(_ | parser(_))

      case None =>
        Fail("Expected nonempty items in orCombinator")
    }

  def orTokenCombinator[Unknown: P, T <: Token](
      prefixCheck: Boolean,
      tokens: Iterator[T]): P[T] =
    orCombinator(
      items = tokens,
      parser = createParser(prefixCheck, _: T)
    )

  @inline private def createParser[Unknown: P, T <: Token](
      prefixCheck: Boolean,
      token: T): P[T] =
    P {
      if (prefixCheck)
        doPrefixCheck(token) ~ parseTokenOnly(token)
      else
        parseTokenOnly(token)
    }

  @inline private def parseTokenOnly[Unknown: P, T <: Token](token: T): P[T] =
    P(token.lexeme) map {
      _ =>
        token
    }

  /** For example, ensure that if the input token is `+` that the parsed token is neither `++` nor `+=` */
  @inline private def doPrefixCheck[Unknown: P, T <: Token](token: T): P[Unit] =
    if (token.otherReservedTokensWithThisPrefix.isEmpty)
      Pass(())
    else
      P(!orTokenCombinator(prefixCheck = true, tokens = token.otherReservedTokensWithThisPrefix.iterator))

}
