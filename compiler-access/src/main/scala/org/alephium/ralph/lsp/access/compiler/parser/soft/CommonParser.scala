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
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

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

  def text[Unknown: P](stop: Token*): P[SoftAST.CodeString] =
    P(Index ~ TokenParser.WhileNotOrFail(stop: _*).! ~ Index) map {
      case (from, text, to) =>
        SoftAST.CodeString(
          text = text,
          index = range(from, to)
        )
    }

  def unresolved[Unknown: P](stop: Option[Token]): P[SoftAST.Unresolved] =
    P {
      Index ~
        CodeParser.parseOrFail(TokenParser.WhileNotOrFail(Seq(Token.Space, Token.Newline) ++ stop: _*).!) ~
        CommentParser.parseOrFail.? ~
        Index
    } map {
      case (from, text, tailComment, to) =>
        SoftAST.Unresolved(
          index = range(from, to),
          code = text,
          documentation = tailComment
        )
    }

  def identifier[Unknown: P](required: Boolean): P[SoftAST.IdentifierAST] =
    if (required)
      identifier
    else
      identifierOrFail

  def identifier[Unknown: P]: P[SoftAST.IdentifierAST] =
    P(Index ~ identifierOrFail.?) map {
      case (_, Some(identifier)) =>
        identifier

      case (from, None) =>
        SoftAST.IdentifierExpected(point(from))
    }

  /**
   * Parses identifiers as long as they are not reserved tokens [[Token.Reserved]].
   *
   * For example, the following code will result in an [[SoftAST.IdentifierExpected]] error
   * because `let` is a reserved token [[Token.Let]]:
   *
   * {{{
   *    fn let() -> ()
   * }}}
   *
   * TODO: Handle cases such as `fn letter() -> ()`
   *
   * @return A successfully parsed identifier instance [[SoftAST.Identifier]] or a parser error.
   */
  def identifierOrFail[Unknown: P]: P[SoftAST.Identifier] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        !TokenParser.Reserved ~
        toCodeOrFail(isLetterDigitOrUnderscore.!) ~
        Index
    } map {
      case (from, documentation, code, to) =>
        SoftAST.Identifier(
          index = range(from, to),
          code = code,
          documentation = documentation
        )
    }

  def toCodeOrFail[Unknown: P](parser: => P[String]): P[SoftAST.CodeString] =
    P(Index ~ parser ~ Index) map {
      case (from, code, to) =>
        SoftAST.CodeString(
          text = code,
          index = range(from, to)
        )
    }

  private def isLetterDigitOrUnderscore[Unknown: P]: P[Unit] =
    CharsWhile {
      char =>
        char.isLetterOrDigit || Token.Underscore.lexeme.contains(char)
    }

}
