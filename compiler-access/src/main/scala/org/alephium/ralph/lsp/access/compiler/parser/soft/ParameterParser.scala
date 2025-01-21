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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private case object ParameterParser {

  def parse[Unknown: P]: P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]] =
    GroupParser.parse(
      open = Token.OpenParen,
      close = Token.CloseParen
    )

  def parse[Unknown: P](required: Boolean): P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]] =
    GroupParser.parse(
      required = required,
      open = Token.OpenParen,
      close = Token.CloseParen
    )

  def parseOrFail[Unknown: P]: P[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]] =
    GroupParser.parseOrFail(
      open = Token.OpenParen,
      close = Token.CloseParen
    )

}
