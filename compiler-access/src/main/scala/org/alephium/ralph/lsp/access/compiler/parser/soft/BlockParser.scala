// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object BlockParser {

  def parse[Unknown: P]: P[SoftAST.BlockClause] =
    parse(required = true)

  def parseOrFail[Unknown: P]: P[SoftAST.BlockClause] =
    parse(required = false)

  def body[Unknown: P]: P[SoftAST.BlockBody] =
    body()

  private def parse[Unknown: P](required: Boolean): P[SoftAST.BlockClause] =
    P {
      Index ~
        TokenParser.parse(required, Token.OpenCurly) ~
        SpaceParser.parseOrFail.? ~
        body(Token.CloseCurly) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.CloseCurly) ~
        Index
    } map {
      case (from, openCurly, preBodySpace, body, postBodySpace, closeCurly, to) =>
        SoftAST.BlockClause(
          index = range(from, to),
          openCurly = openCurly,
          preBodySpace = preBodySpace,
          body = body,
          postBodySpace = postBodySpace,
          closeCurly = closeCurly
        )
    }

  private def body[Unknown: P](stop: Token*): P[SoftAST.BlockBody] =
    P {
      Index ~
        SpaceParser.parseOrFail.? ~
        bodyPart(stop).rep ~
        Index
    } map {
      case (from, headSpace, parts, to) =>
        SoftAST.BlockBody(
          index = range(from, to),
          prePartsSpace = headSpace,
          parts = parts
        )
    }

  private def bodyPart[Unknown: P](stop: Seq[Token]): P[SoftAST.BlockBodyPart] =
    P {
      Index ~
        part(stop) ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, ast, space, to) =>
        SoftAST.BlockBodyPart(
          index = range(from, to),
          part = ast,
          postPartSpace = space
        )
    }

  private def part[Unknown: P](stop: Seq[Token]): P[SoftAST.BodyPartAST] =
    P {
      TemplateParser.parseOrFail |
        EventParser.parseOrFail |
        StructParser.parseOrFail |
        FunctionParser.parseOrFail |
        ExpressionParser.parseOrFail |
        CommentParser.parseOrFail |
        UnresolvedParser.parseOrFail(stop: _*)
    }

}
