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
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TupleParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Tuple] =
    P(tuple(required = false))

  def parse[Unknown: P]: P[SoftAST.Tuple] =
    P(tuple(required = true))

  def parse[Unknown: P](required: Boolean): P[SoftAST.Tuple] =
    P(tuple(required))

  /**
   * Parses a sequence of arguments enclosed within parentheses.
   *
   * Syntax: (arg1, arg2, (arg3, arg4), arg5)
   *
   * @param required Determines if the parser should fail when the opening parenthesis is missing.
   * @return An instance of [[SoftAST.Tuple]].
   */
  private def tuple[Unknown: P](required: Boolean): P[SoftAST.Tuple] =
    P {
      Index ~
        TokenParser.parse(required, Token.OpenParen) ~
        spaceOrFail.? ~
        Index ~
        ExpressionParser.parseOrFail.? ~
        spaceOrFail.? ~
        tailParams.rep ~
        TokenParser.parse(Token.CloseParen) ~
        Index
    } map {
      case (from, openParen, preHeadSpace, headParamIndex, headExpression, postHeadSpace, tailParams, closeParen, to) =>
        // Look ahead check: If tail param is provided, but head param is missing, report the head param as required.
        // For example, in the case of `(, tailArgs)`, head param is missing which is required.
        val headExpressionAdjusted =
          if (tailParams.nonEmpty && headExpression.isEmpty)
            Some(SoftAST.ExpressionExpected(point(headParamIndex)))
          else
            headExpression

        SoftAST.Tuple(
          index = range(from, to),
          openParen = openParen,
          preHeadExpressionSpace = preHeadSpace,
          headExpression = headExpressionAdjusted,
          postHeadExpressionSpace = postHeadSpace,
          tailExpressions = tailParams,
          closeParen = closeParen
        )

    }

  /**
   * Parses a "tail param" which may contain nested arguments.
   *
   * Syntax: (arg1>>, arg2, (arg3, arg4), arg5<<)
   *
   * @return An instance of [[SoftAST.TupleTail]].
   */
  private def tailParams[Unknown: P]: P[SoftAST.TupleTail] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Comma) ~
        spaceOrFail.? ~
        ExpressionParser.parse ~
        spaceOrFail.? ~
        Index
    } map {
      case (from, comma, preParamNameSpace, argumentName, postParamNameSpace, to) =>
        SoftAST.TupleTail(
          index = range(from, to),
          comma = comma,
          preExpressionSpace = preParamNameSpace,
          expression = argumentName,
          postExpressionSpace = postParamNameSpace
        )
    }

}
