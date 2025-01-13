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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object CommonParser {

  def space[Unknown: P]: P[SoftAST.SpaceAST] =
    P(Index ~ spaceOrFail.?) map {
      case (_, Some(space)) =>
        space

      case (from, None) =>
        SoftAST.SpaceExpected(range(from, from))
    }

  def spaceOrFail[Unknown: P]: P[SoftAST.Space] =
    P(toCodeOrFail(CharsWhileIn(" \t\r\n").!)) map {
      text =>
        SoftAST.Space(text)
    }

  def toCodeOrFail[Unknown: P](parser: => P[String]): P[SoftAST.CodeString] =
    P(Index ~ parser ~ Index) map {
      case (from, code, to) =>
        SoftAST.CodeString(
          text = code,
          index = range(from, to)
        )
    }

}
