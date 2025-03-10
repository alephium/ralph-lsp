// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object AccessModifierParser {

  def parseOrFail[Unknown: P]: P[SoftAST.AccessModifier] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Pub) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, pub, space, to) =>
        SoftAST.AccessModifier(
          index = range(from, to),
          pub = pub,
          postTokenSpace = space
        )
    }

}
