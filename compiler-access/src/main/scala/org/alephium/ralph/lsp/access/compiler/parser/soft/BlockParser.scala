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

  /**
   * Parses a block's body such a parent block is already defined.
   * For example, within a parent contract or a function.
   *
   * @return Parsed content of a block.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Block] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.OpenCurly) ~
        BlockPartParser.parseOrFail(blockExpressions = false, stop = Token.CloseCurly).rep ~
        TokenParser.parse(Token.CloseCurly) ~
        Index
    } map {
      case (from, openCurly, parts, closeCurly, to) =>
        SoftAST.Block(
          index = range(from, to),
          openCurly = openCurly,
          parts = parts,
          closeCurly = closeCurly
        )
    }

}
