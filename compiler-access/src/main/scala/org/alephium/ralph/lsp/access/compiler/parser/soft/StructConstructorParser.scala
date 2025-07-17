// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object StructConstructorParser {

  val stops: List[Token] =
    TypeAssignmentGroupParser.stops(Token.CloseCurly)

  def parseOrFail[Unknown: P]: P[SoftAST.StructConstructor] =
    P {
      Index ~
        IdentifierParser.parseOrFail ~
        SpaceParser.parseOrFail.? ~
        GroupParser.parseOrFail(
          open = Token.OpenCurly,
          close = Token.CloseCurly,
          delimiter = Token.Comma,
          expressionsParseOrFail = expressions
        ) ~
        Index
    } map {
      case (from, identifier, preParamSpace, params, to) =>
        SoftAST.StructConstructor(
          index = range(from, to),
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params
        )
    }

  private def expressions[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      StructFieldAssignmentParser.parse |
        UnresolvedParser.parseOrFail(stops: _*)
    }

}
