// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object GroupParser {

  def parse[Unknown: P, O <: Token, C <: Token](
      open: O,
      close: C,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[O, C]] =
    P {
      group(
        required = true,
        open = open,
        close = close,
        expressionsParseOrFail = expressionsParseOrFail
      )
    }

  def parse[Unknown: P, O <: Token, C <: Token](
      required: Boolean,
      open: O,
      close: C,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[O, C]] =
    P {
      group(
        required,
        open = open,
        close = close,
        expressionsParseOrFail = expressionsParseOrFail
      )
    }

  def parseOrFail[Unknown: P, O <: Token, C <: Token](
      open: O,
      close: C,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[O, C]] =
    P {
      group(
        required = false,
        open = open,
        close = close,
        expressionsParseOrFail = expressionsParseOrFail
      )
    }

  /**
   * Parses a sequence of comma separated expressions.
   *
   * Syntax: expr1, expr2, (expr3, expr4), ...
   *
   * @return An instance of [[SoftAST.Group]] without enclosing tokens.
   */
  def parseOrFail[Unknown: P](expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[Nothing, Nothing]] =
    P {
      Index ~
        expressionsParseOrFail.? ~
        SpaceParser.parseOrFail.? ~
        tail(expressionsParseOrFail).rep(1) ~
        Index
    } map {
      case (from, headExpression, postHeadSpace, tailParams, to) =>
        val headExpressionAdjusted =
          adjustHeadExpression(
            headParamIndex = from,
            headExpression = headExpression,
            tailParams = tailParams
          )

        SoftAST.Group(
          index = range(from, to),
          openToken = None,
          preHeadExpressionSpace = None,
          headExpression = headExpressionAdjusted,
          postHeadExpressionSpace = postHeadSpace,
          tailExpressions = tailParams,
          closeToken = None
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
      close: C,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[O, C]] =
    P {
      Index ~
        TokenParser.parse(required, open) ~
        SpaceParser.parseOrFail.? ~
        Index ~
        expressionsParseOrFail.? ~
        SpaceParser.parseOrFail.? ~
        tail(expressionsParseOrFail).rep ~
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
  private def tail[Unknown: P](expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.GroupTail] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Comma) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expressionsParseOrFail) ~
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

  /**
   * TODO: This is a temporary placeholder.
   *       It should be replaced as a function input parameter.
   */
  def defaultExpressions[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      TypeAssignmentParser.parseOrFail |
        AssignmentParser.parseOrFail |
        InfixCallParser.parseOrFail |
        MethodCallParser.parseOrFail |
        MutableBindingParser.parseOrFail |
        IfElseParser.parseOrFail |
        ElseParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        TupleParser.parseOrFail |
        ArrayParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        ArrayAccessParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
