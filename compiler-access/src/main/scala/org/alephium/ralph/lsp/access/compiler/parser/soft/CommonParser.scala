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

private object CommonParser {

  def spaceOrFail[Unknown: P]: P[SoftAST.Space] =
    P(Index ~ CharsWhileIn(" \t\r\n").! ~ Index) map {
      case (from, text, to) =>
        SoftAST.Space(
          code = text,
          index = range(from, to)
        )
    }

  def space[Unknown: P]: P[SoftAST.SpaceAST] =
    P(Index ~ spaceOrFail.?) map {
      case (_, Some(space)) =>
        space

      case (from, None) =>
        SoftAST.SpaceExpected(range(from, from))
    }

  def text[Unknown: P]: P[SoftAST.Text] =
    P(Index ~ CharsWhileNot(Token.Newline.lexeme).! ~ Index) map {
      case (from, text, to) =>
        SoftAST.Text(
          code = text,
          index = range(from, to)
        )
    }

  def unresolved[Unknown: P](stopChars: Option[String]): P[SoftAST.Unresolved] =
    P(Index ~ CharsWhileNot(Token.Space.lexeme ++ Token.Newline.lexeme ++ stopChars.getOrElse("")).! ~ Index) map {
      case (from, text, to) =>
        SoftAST.Unresolved(
          code = text,
          index = range(from, to)
        )
    }

  def identifier[Unknown: P]: P[SoftAST.IdentifierAST] =
    P(Index ~ isLetterDigitOrUnderscore.!.? ~ Index) map {
      case (from, Some(identifier), to) =>
        SoftAST.Identifier(
          code = identifier,
          index = range(from, to)
        )

      case (from, None, to) =>
        SoftAST.IdentifierExpected(range(from, to))
    }

  def namedArgumentOrFail[Unknown: P]: P[SoftAST.Argument] =
    P(Index ~ isLetterDigitOrUnderscore.! ~ Index) map {
      case (from, argument, to) =>
        SoftAST.Argument(
          code = argument,
          index = range(from, to)
        )
    }

  def typeName[Unknown: P]: P[SoftAST.SingleTypeAST] =
    P(Index ~ isLetterDigitOrUnderscore.!.? ~ Index) map {
      case (from, Some(typeName), to) =>
        SoftAST.Type(
          code = typeName,
          index = range(from, to)
        )

      case (from, None, to) =>
        SoftAST.TypeExpected(range(from, to))
    }

  private def CharsWhileNot[Unknown: P](chars: String): P[Unit] =
    CharsWhile {
      char =>
        !(chars contains char)
    }

  private def isLetterDigitOrUnderscore[Unknown: P]: P[Unit] =
    CharsWhile {
      char =>
        char.isLetterOrDigit || char == Token.Underscore.lexeme.head
    }

}
