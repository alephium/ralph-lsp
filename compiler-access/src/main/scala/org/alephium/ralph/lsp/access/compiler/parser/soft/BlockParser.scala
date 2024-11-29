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
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object BlockParser {

  def clause[Unknown: P](required: Boolean): P[SoftAST.BlockClause] =
    P(Index ~ TokenParser.openCurly(required) ~ spaceOrFail.? ~ body(Some(Token.CloseCurly.lexeme)) ~ spaceOrFail.? ~ TokenParser.closeCurly ~ Index) map {
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

  def body[Unknown: P]: P[SoftAST.BlockBody] =
    body(stopChars = None)

  private def body[Unknown: P](stopChars: Option[String]): P[SoftAST.BlockBody] =
    P(Index ~ spaceOrFail.? ~ bodyPart(stopChars).rep ~ Index) map {
      case (from, headSpace, parts, to) =>
        SoftAST.BlockBody(
          index = range(from, to),
          prePartsSpace = headSpace,
          parts = parts
        )
    }

  private def bodyPart[Unknown: P](stopChars: Option[String]): P[SoftAST.BlockBodyPart] =
    P(Index ~ part(stopChars) ~ spaceOrFail.? ~ Index) map {
      case (from, ast, space, to) =>
        SoftAST.BlockBodyPart(
          index = range(from, to),
          part = ast,
          postPartSpace = space
        )
    }

  private def part[Unknown: P](stopChars: Option[String]): P[SoftAST.BodyPartAST] =
    P {
      TemplateParser.parse |
        FunctionParser.parse |
        CommentParser.parse |
        unresolved(stopChars)
    }

}
