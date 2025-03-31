// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object CodeParser {

  def parseOrFail[Unknown: P, T <: Token](token: T): P[SoftAST.CodeToken[T]] =
    P {
      Index ~
        // Ensure that the input token is not a prefix of another token.
        // For example, if the input token is `+`, ensure that the parsed token does not match `+=` or `++`.
        !TokenParser.WhileInOrFail(token.otherReservedTokensWithThisPrefix) ~
        token.lexeme ~
        Index
    } map {
      case (from, to) =>
        SoftAST.CodeToken(
          index = range(from, to),
          token = token
        )
    }

  def parseOrFail[Unknown: P](parser: => P[String]): P[SoftAST.CodeString] =
    P(Index ~ parser ~ Index) map {
      case (from, code, to) =>
        SoftAST.CodeString(
          index = range(from, to),
          text = code
        )
    }

}
