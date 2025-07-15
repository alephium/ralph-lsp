// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TypeAssignmentGroupParser {

  private def stops[C <: Token](close: C) = {
    // In struct's case, the tokens 'space, comma, close' are likely to occur to more often than other tokens.
    // Therefore, process them first before the others.
    val head = Token.spaces :+ Token.Comma :+ close
    val tail = Token.reserved.diff(head)
    head ++ tail
  }

  def parse[Unknown: P, O <: Token, C <: Token](
      open: O,
      close: C): P[SoftAST.Group[O, C]] =
    GroupParser.parse(
      open = open,
      close = close,
      expressionsParseOrFail = groupExpression(close)
    )

  def parseOrFail[Unknown: P, O <: Token, C <: Token](
      open: O,
      close: C): P[SoftAST.Group[O, C]] =
    GroupParser.parseOrFail(
      open = open,
      close = close,
      expressionsParseOrFail = groupExpression(close)
    )

  private def groupExpression[Unknown: P, C <: Token](close: C): P[SoftAST.ExpressionAST] =
    P(TypeAssignmentParser.parse | UnresolvedParser.parseOrFail(stops(close): _*))

}
