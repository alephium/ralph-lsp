// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.util

import fastparse._
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

  def orTokenCombinator[Unknown: P, T <: Token](tokens: Iterator[T]): P[T] =
    orCombinator(
      items = tokens,
      parser = createParser(_: T)
    )

  @inline private def createParser[Unknown: P, T <: Token](token: T): P[T] =
    P(token.lexeme) map {
      _ =>
        token
    }

}
