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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object ArgumentParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Arguments] =
    P(arguments(required = false))

  def parse[Unknown: P]: P[SoftAST.Arguments] =
    P(arguments(required = true))

  def parse[Unknown: P](required: Boolean): P[SoftAST.Arguments] =
    P(arguments(required))

  /**
   * Parses a sequence of arguments enclosed within parentheses.
   *
   * Syntax: (arg1, arg2, (arg3, arg4), arg5)
   *
   * @param required Determines if the parser should fail when the opening parenthesis is missing.
   * @return An instance of [[SoftAST.Arguments]].
   */
  private def arguments[Unknown: P](required: Boolean): P[SoftAST.Arguments] =
    P(Index ~ TokenParser.openParen(required) ~ spaceOrFail.? ~ Index ~ tupledOrNamedOrFail.? ~ tailArgument.rep ~ TokenParser.closeParen ~ Index) map {
      // if tail argument is provided, but head argument is missing, report the head argument as required
      case (from, openParen, preHeadArgumentSpace, headArgumentIndex, None, tailArguments, closeParen, to) if tailArguments.nonEmpty =>
        SoftAST.Arguments(
          index = range(from, to),
          openParen = openParen,
          preHeadArgumentSpace = preHeadArgumentSpace,
          headArgument = Some(SoftAST.ArgumentExpected(range(headArgumentIndex, headArgumentIndex))),
          tailArguments = tailArguments,
          closeParen = closeParen
        )

      case (from, openParen, preHeadArgumentSpace, _, headArgument, tailArguments, closeParen, to) =>
        SoftAST.Arguments(
          index = range(from, to),
          openParen = openParen,
          preHeadArgumentSpace = preHeadArgumentSpace,
          headArgument = headArgument,
          tailArguments = tailArguments,
          closeParen = closeParen
        )
    }

  /**
   * Parses a "tail argument" which may contain nested arguments.
   *
   * Syntax: (arg1>>, arg2, (arg3, arg4), arg5<<)
   *
   * @return An instance of [[SoftAST.TailArgument]].
   */
  private def tailArgument[Unknown: P]: P[SoftAST.TailArgument] =
    P(Index ~ TokenParser.commaOrFail ~ spaceOrFail.? ~ tupledOrNamedExpected ~ spaceOrFail.? ~ Index) map {
      case (from, comma, preArgumentNameSpace, argumentName, postArgumentNameSpace, to) =>
        SoftAST.TailArgument(
          index = range(from, to),
          comma = comma,
          preArgumentSpace = preArgumentNameSpace,
          argument = argumentName,
          postArgumentSpace = postArgumentNameSpace
        )
    }

  private def tupledOrNamedExpected[Unknown: P]: P[SoftAST.ArgumentAST] =
    P(Index ~ tupledOrNamedOrFail.? ~ Index) map {
      case (_, Some(argument), _) =>
        argument

      case (from, None, to) =>
        SoftAST.ArgumentExpected(range(from, to))
    }

  private def tupledOrNamedOrFail[Unknown: P]: P[SoftAST.NonEmptyArguments] =
    P(parseOrFail | namedArgumentOrFail)

}
