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

private object IfElseParser {

  def parseOrFail[Unknown: P]: P[SoftAST.IfElse] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.If) ~
        TokenParser.isBoundary(Token.OpenParen, Token.OpenCurly) ~
        SpaceParser.parseOrFail.? ~
        ParameterParser.parse ~
        SpaceParser.parseOrFail.? ~
        BlockParser.parseOrFail.? ~
        // do not read the space if `else` is not provided
        (SpaceParser.parseOrFail.? ~ ElseParser.parseOrFail).? ~
        Index
    } map {
      case (from, ifToken, preGroupSpace, group, preBlockSpace, block, elseStatement, to) =>
        val (elseSpace, elseAST) =
          elseStatement match {
            case Some((space, statement)) =>
              (space, Some(statement))

            case None =>
              (None, None)
          }

        SoftAST.IfElse(
          index = range(from, to),
          ifToken = ifToken,
          preGroupSpace = preGroupSpace,
          group = group,
          preBlockSpace = preBlockSpace,
          block = block,
          preElseSpace = elseSpace,
          elseStatement = elseAST
        )
    }

}
