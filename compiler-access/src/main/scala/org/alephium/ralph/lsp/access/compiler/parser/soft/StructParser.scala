// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object StructParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Struct] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Struct) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        TypeAssignmentGroupParser.parse(Token.OpenCurly, Token.CloseCurly) ~
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

}
