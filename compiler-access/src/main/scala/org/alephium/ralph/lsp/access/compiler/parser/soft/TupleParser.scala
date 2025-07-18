// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TupleParser {

  def parse[Unknown: P]: P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type, Token.Comma.type]] =
    GroupParser.parse(
      open = Token.OpenParen,
      close = Token.CloseParen,
      delimiter = Token.Comma,
      expressionsParseOrFail = GroupParser.defaultExpressions
    )

  def parse[Unknown: P](required: Boolean): P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type, Token.Comma.type]] =
    GroupParser.parse(
      required = required,
      open = Token.OpenParen,
      close = Token.CloseParen,
      delimiter = Token.Comma,
      expressionsParseOrFail = GroupParser.defaultExpressions
    )

  def parseOrFail[Unknown: P](assertNonEmpty: Boolean): P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type, Token.Comma.type]] =
    GroupParser.parseOrFail(
      assertNonEmpty = assertNonEmpty,
      open = Token.OpenParen,
      close = Token.CloseParen,
      delimiter = Token.Comma,
      expressionsParseOrFail = GroupParser.defaultExpressions
    )

}
