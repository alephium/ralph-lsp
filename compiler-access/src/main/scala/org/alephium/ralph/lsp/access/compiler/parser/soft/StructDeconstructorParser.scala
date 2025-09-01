// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object StructDeconstructorParser {

  def parseOrFail[Unknown: P]: P[SoftAST.StructDeconstructor] =
    P {
      Index ~
        IdentifierParser.parseOrFail ~
        SpaceParser.parseOrFail.? ~
        GroupParser.parseOrFail(
          assertNonEmpty = false,
          open = Token.OpenCurly,
          close = Token.CloseCurly,
          delimiter = Token.Comma,
          expressionsParseOrFail = expressions
        ) ~
        Index
    } map {
      case (from, identifier, preParamSpace, params, to) =>
        SoftAST.StructDeconstructor(
          index = range(from, to),
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params
        )
    }

  private def expressions[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      StructDeconstructorFieldParser.parseOrFail |
        UnresolvedParser.parseOrFail(StructConstructorParser.stops: _*)
    }

}
