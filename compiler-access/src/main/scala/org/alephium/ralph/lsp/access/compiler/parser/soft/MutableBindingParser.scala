// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object MutableBindingParser {

  /** Syntax: mut [identifier] */
  def parseOrFail[Unknown: P]: P[SoftAST.MutableBinding] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Mut) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        Index
    } map {
      case (from, mut, space, identifier, to) =>
        SoftAST.MutableBinding(
          index = range(from, to),
          mut = mut,
          space = space,
          identifier = identifier
        )
    }

}
