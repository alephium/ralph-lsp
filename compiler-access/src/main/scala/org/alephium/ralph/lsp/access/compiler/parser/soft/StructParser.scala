// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object StructParser {

  private val stops = {
    // In struct's case, the tokens 'space, comma, close-curly' are likely to occur to more often than other tokens.
    // Therefore, process them first before the others.
    val head = Token.spaces :+ Token.Comma :+ Token.CloseCurly
    val tail = Token.reserved
    (head ++ tail).distinct
  }

  def parseOrFail[Unknown: P]: P[SoftAST.Struct] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Struct) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        GroupParser.parse(
          open = Token.OpenCurly,
          close = Token.CloseCurly,
          expressionsParseOrFail = expressions
        ) ~
        Index
    } map {
      case (from, structToken, preIdentifierSpace, identifier, preParamSpace, params, to) =>
        SoftAST.Struct(
          index = range(from, to),
          structToken = structToken,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params
        )
    }

  private def expressions[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      TypeAssignmentParser.parse | // A type assignment is required
        UnresolvedParser.parseOrFail(stops: _*)
    }

}
