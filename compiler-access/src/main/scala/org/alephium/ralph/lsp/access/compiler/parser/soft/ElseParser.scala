// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object ElseParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Else] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Else) ~
        TokenParser.isBoundary(Token.OpenParen, Token.OpenCurly) ~
        SpaceParser.parseOrFail.? ~
        IfElseParser.parseBody ~
        Index
    } map {
      case (from, elseToken, preBodySpace, body, to) =>
        SoftAST.Else(
          index = range(from, to),
          elseToken = elseToken,
          preBodySpace = preBodySpace,
          body = body
        )
    }

}
