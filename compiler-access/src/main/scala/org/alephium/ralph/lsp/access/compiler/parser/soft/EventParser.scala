// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object EventParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Event] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Event) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        TupleParser.parse ~
        Index
    } map {
      case (from, eventToken, preIdentifierSpace, identifier, preParamSpace, params, to) =>
        SoftAST.Event(
          index = range(from, to),
          eventToken = eventToken,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params
        )
    }

}
