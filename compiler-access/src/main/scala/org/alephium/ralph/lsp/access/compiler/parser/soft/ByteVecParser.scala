// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object ByteVecParser {

  def parseOrFail[Unknown: P]: P[SoftAST.ByteVec] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Hash) ~
        CodeParser.parseOrFail(hexString).? ~
        Index
    } map {
      case (from, hash, hex, to) =>
        SoftAST.ByteVec(
          index = range(from, to),
          hash = hash,
          hex = hex
        )
    }

  private def hexString[Unknown: P]: P[String] =
    P(CharIn("0-9a-zA-Z").rep(1).!)

}
