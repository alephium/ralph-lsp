// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TupleParser {

  def parse[Unknown: P]: P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]] =
    GroupParser.parse(
      open = Token.OpenParen,
      close = Token.CloseParen,
      expressionsParseOrFail = GroupParser.defaultExpressions
    )

  def parse[Unknown: P](required: Boolean): P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]] =
    GroupParser.parse(
      required = required,
      open = Token.OpenParen,
      close = Token.CloseParen,
      expressionsParseOrFail = GroupParser.defaultExpressions
    )

  def parseOrFail[Unknown: P]: P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]] =
    GroupParser.parseOrFail(
      open = Token.OpenParen,
      close = Token.CloseParen,
      expressionsParseOrFail = GroupParser.defaultExpressions
    )

}
