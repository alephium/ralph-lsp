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

private object TokenParser {

  def colon[Unknown: P]: P[SoftAST.ColonAST] =
    P(Index ~ Token.Colon.lexeme.!.? ~ Index) map {
      case (from, Some(_), to) =>
        SoftAST.Colon(range(from, to))

      case (from, None, to) =>
        SoftAST.ColonExpected(range(from, to))
    }

  def openParen[Unknown: P](required: Boolean): P[SoftAST.OpenParenAST] =
    if (required)
      openParen
    else
      openParenOrFail

  def openParen[Unknown: P]: P[SoftAST.OpenParenAST] =
    P(Index ~ Token.OpenParen.lexeme.!.? ~ Index) map {
      case (from, Some(_), to) =>
        SoftAST.OpenParen(range(from, to))

      case (from, None, to) =>
        SoftAST.OpenParenExpected(range(from, to))
    }

  def openParenOrFail[Unknown: P]: P[SoftAST.OpenParen] =
    P(Index ~ Token.OpenParen.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.OpenParen(range(from, to))
    }

  def closeParen[Unknown: P]: P[SoftAST.CloseParenAST] =
    P(Index ~ Token.CloseParen.lexeme.!.? ~ Index) map {
      case (from, Some(_), to) =>
        SoftAST.CloseParen(range(from, to))

      case (from, None, to) =>
        SoftAST.CloseParenExpected(range(from, to))
    }

  def closeParenOrFail[Unknown: P]: P[SoftAST.CloseParenAST] =
    P(Index ~ Token.CloseParen.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.CloseParen(range(from, to))
    }

  def openCurly[Unknown: P](required: Boolean): P[SoftAST.OpenCurlyAST] =
    if (required)
      openCurly
    else
      openCurlyOrFail

  def openCurly[Unknown: P]: P[SoftAST.OpenCurlyAST] =
    P(Index ~ Token.OpenCurly.lexeme.!.? ~ Index) map {
      case (from, Some(_), to) =>
        SoftAST.OpenCurly(range(from, to))

      case (from, None, to) =>
        SoftAST.OpenCurlyExpected(range(from, to))
    }

  def openCurlyOrFail[Unknown: P]: P[SoftAST.OpenCurly] =
    P(Index ~ Token.OpenCurly.lexeme.! ~ Index) map {
      case (from, _, to) =>
        SoftAST.OpenCurly(range(from, to))
    }

  def closeCurly[Unknown: P]: P[SoftAST.CloseCurlyAST] =
    P(Index ~ Token.CloseCurly.lexeme.!.? ~ Index) map {
      case (from, Some(_), to) =>
        SoftAST.CloseCurly(range(from, to))

      case (from, None, to) =>
        SoftAST.CloseCurlyExpected(range(from, to))
    }

  def commaOrFail[Unknown: P]: P[SoftAST.Comma] =
    P(Index ~ Token.Comma.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.Comma(range(from, to))
    }

  def forwardArrow[Unknown: P]: P[SoftAST.ForwardArrowAST] =
    P(Index ~ Token.ForwardArrow.lexeme.!.? ~ Index) map {
      case (from, Some(_), to) =>
        SoftAST.ForwardArrow(range(from, to))

      case (from, None, to) =>
        SoftAST.ForwardArrowExpected(range(from, to))
    }

  def doubleForwardSlashOrFail[Unknown: P]: P[SoftAST.DoubleForwardSlash] =
    P(Index ~ Token.DoubleForwardSlash.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.DoubleForwardSlash(range(from, to))
    }

  def ContractOrFail[Unknown: P]: P[SoftAST.Contract] =
    P(Index ~ Token.Contract.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.Contract(range(from, to))
    }

  def TxScriptOrFail[Unknown: P]: P[SoftAST.TxScript] =
    P(Index ~ Token.TxScript.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.TxScript(range(from, to))
    }

  def FnOrFail[Unknown: P]: P[SoftAST.Fn] =
    P(Index ~ Token.Fn.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.Fn(range(from, to))
    }

  def Implements[Unknown: P]: P[SoftAST.Implements] =
    P(Index ~ Token.Implements.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.Implements(range(from, to))
    }

  def Extends[Unknown: P]: P[SoftAST.Extends] =
    P(Index ~ Token.Extends.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.Extends(range(from, to))
    }

}
