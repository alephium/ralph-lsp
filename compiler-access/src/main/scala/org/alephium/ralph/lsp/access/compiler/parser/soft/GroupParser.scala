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

private object GroupParser {

  def parse[Unknown: P, O <: Token, C <: Token](
      open: O,
      close: C): P[SoftAST.Group[O, C]] =
    P {
      group(
        required = true,
        open = open,
        close = close
      )
    }

  def parse[Unknown: P, O <: Token, C <: Token](
      required: Boolean,
      open: O,
      close: C): P[SoftAST.Group[O, C]] =
    P {
      group(
        required,
        open = open,
        close = close
      )
    }

  def parseOrFail[Unknown: P, O <: Token, C <: Token](
      open: O,
      close: C): P[SoftAST.Group[O, C]] =
    P {
      group(
        required = false,
        open = open,
        close = close
      )
    }

  /**
   * Parses a sequence of expressions enclosed within the given open and close tokens.
   *
   * Syntax: (expr1, expr2, (expr3, expr4), ...)
   *
   * @param required Determines if the parser should fail when the opening parenthesis is missing.
   * @return An instance of [[SoftAST.Group]].
   */
  private def group[Unknown: P, O <: Token, C <: Token](
      required: Boolean,
      open: O,
      close: C): P[SoftAST.Group[O, C]] =
    P {
      Index ~
        TokenParser.parse(required, open) ~
        SpaceParser.parseOrFail.? ~
        Index ~
        expression(open, close).? ~
        SpaceParser.parseOrFail.? ~
        tail(open, close).rep ~
        TokenParser.parse(close) ~
        Index
    } map {
      case (from, openParen, preHeadSpace, headParamIndex, headExpression, postHeadSpace, tailParams, closeParen, to) =>
        val headExpressionAdjusted =
          adjustHeadExpression(
            headParamIndex = headParamIndex,
            headExpression = headExpression,
            tailParams = tailParams
          )

        SoftAST.Group(
          index = range(from, to),
          openToken = Some(openParen),
          preHeadExpressionSpace = preHeadSpace,
          headExpression = headExpressionAdjusted,
          postHeadExpressionSpace = postHeadSpace,
          tailExpressions = tailParams,
          closeToken = Some(closeParen)
        )

    }

  /**
   * Look ahead check: If tail param is provided, but head param is missing, report the head param as required.
   *
   * For example, in the case of `(, tailArgs)`, head param is missing which is required.
   */
  private def adjustHeadExpression(
      headParamIndex: Int,
      headExpression: Option[SoftAST.ExpressionAST],
      tailParams: Seq[SoftAST.GroupTail]): Option[SoftAST.ExpressionAST] =
    if (tailParams.nonEmpty && headExpression.isEmpty)
      Some(SoftAST.ExpressionExpected(point(headParamIndex)))
    else
      headExpression

  /**
   * Parses a "tail param" which may contain nested arguments.
   *
   * Syntax: (arg1>>, arg2, (arg3, arg4), arg5<<)
   *
   * @return An instance of [[SoftAST.GroupTail]].
   */
  private def tail[Unknown: P, O <: Token, C <: Token](
      open: O,
      close: C): P[SoftAST.GroupTail] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Comma) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expression(open, close)) ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, comma, preParamNameSpace, argumentName, postParamNameSpace, to) =>
        SoftAST.GroupTail(
          index = range(from, to),
          comma = comma,
          preExpressionSpace = preParamNameSpace,
          expression = argumentName,
          postExpressionSpace = postParamNameSpace
        )
    }

  private def expression[Unknown: P, O <: Token, C <: Token](
      open: O,
      close: C): P[SoftAST.ExpressionAST] =
    P {
      GroupParser.parseOrFail(open, close) |
        TypeAssignmentParser.parseOrFail |
        AssignmentParser.parseOrFail |
        InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
