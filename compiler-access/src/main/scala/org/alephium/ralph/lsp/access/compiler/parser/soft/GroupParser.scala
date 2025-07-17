// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{point, range}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object GroupParser {

  def parse[Unknown: P, O <: Token, C <: Token, D <: Token](
      open: O,
      close: C,
      delimiter: D,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[O, C, D]] =
    P {
      group(
        required = true,
        assertNonEmpty = false,
        open = open,
        close = close,
        delimiter = delimiter,
        expressionsParseOrFail = expressionsParseOrFail
      )
    }

  def parse[Unknown: P, O <: Token, C <: Token, D <: Token](
      required: Boolean,
      open: O,
      close: C,
      delimiter: D,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[O, C, D]] =
    P {
      group(
        required = required,
        assertNonEmpty = false,
        open = open,
        close = close,
        delimiter = delimiter,
        expressionsParseOrFail = expressionsParseOrFail
      )
    }

  def parseOrFail[Unknown: P, O <: Token, C <: Token, D <: Token](
      assertNonEmpty: Boolean,
      open: O,
      close: C,
      delimiter: D,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[O, C, D]] =
    P {
      group(
        required = false,
        assertNonEmpty = assertNonEmpty,
        open = open,
        close = close,
        delimiter = delimiter,
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
  def parseOrFail[Unknown: P, D <: Token](
      delimiter: D,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[Nothing, Nothing, D]] =
    P {
      Index ~
        expressionsParseOrFail ~
        // Tail space must only be read if a tail expression exists
        (SpaceParser.parseOrFail.? ~ tail(delimiter, expressionsParseOrFail).rep(1)).? ~
        Index
    } map {
      case (from, headExpression, tail, to) =>
        val (preTailExpressionSpace, tailExpressions) =
          tail match {
            case Some((space, tail)) =>
              (space, tail)

            case None =>
              (None, Seq.empty)
          }

        SoftAST.Group(
          index = range(from, to),
          openToken = None,
          preHeadExpressionSpace = None,
          headExpression = Some(headExpression),
          preTailExpressionSpace = preTailExpressionSpace,
          tailExpressions = tailExpressions,
          closeToken = None
        )
    }

  /**
   * Requires at least two expression for a successful parse.
   *
   * This is primarily used by [[ReturnParser]] for cases where a tuple is returned.
   * If it's not a tuple, then a group with single element should not be created, instead,
   * it should get processed by [[ReturnParser]] as a [[SoftAST.Identifier]].
   *
   * @return An instance of [[SoftAST.Group]] without enclosing tokens.
   */
  def parseOrFailMany[Unknown: P](expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[Nothing, Nothing, Token.Comma.type]] =
    P {
      Index ~
        expressionsParseOrFail.? ~
        SpaceParser.parseOrFail.? ~
        tail(Token.Comma, expressionsParseOrFail).rep(1) ~
        Index
    } map {
      case (from, headExpression, postHeadSpace, tailParams, to) =>
        val headExpressionAdjusted =
          adjustHeadExpression(
            assertNonEmpty = false,
            headParamIndex = from,
            headExpression = headExpression,
            tailParams = tailParams
          )

        SoftAST.Group(
          index = range(from, to),
          openToken = None,
          preHeadExpressionSpace = None,
          headExpression = headExpressionAdjusted,
          preTailExpressionSpace = postHeadSpace,
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
  private def group[Unknown: P, O <: Token, C <: Token, D <: Token](
      required: Boolean,
      assertNonEmpty: Boolean,
      open: O,
      close: C,
      delimiter: D,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.Group[O, C, D]] =
    P {
      Index ~
        TokenParser.parse(required, open) ~
        SpaceParser.parseOrFail.? ~
        Index ~
        expressionsParseOrFail.? ~
        SpaceParser.parseOrFail.? ~
        tail(delimiter, expressionsParseOrFail).rep ~
        TokenParser.parse(close) ~
        Index
    } map {
      case (from, openParen, preHeadSpace, headParamIndex, headExpression, postHeadSpace, tailParams, closeParen, to) =>
        val headExpressionAdjusted =
          adjustHeadExpression(
            assertNonEmpty = assertNonEmpty,
            headParamIndex = headParamIndex,
            headExpression = headExpression,
            tailParams = tailParams
          )

        SoftAST.Group(
          index = range(from, to),
          openToken = Some(openParen),
          preHeadExpressionSpace = preHeadSpace,
          headExpression = headExpressionAdjusted,
          preTailExpressionSpace = postHeadSpace,
          tailExpressions = tailParams,
          closeToken = Some(closeParen)
        )

    }

  /**
   * Look ahead check: If tail param is provided, but head param is missing, report the head param as required.
   *
   * For example, in the case of `(, tailArgs)`, head param is missing which is required.
   *
   * @param assertNonEmpty If `true`, asserts that the group contains at least one element.
   *                       Reports an error at the opening token if empty.
   */
  private def adjustHeadExpression[D <: Token](
      assertNonEmpty: Boolean,
      headParamIndex: Int,
      headExpression: Option[SoftAST.ExpressionAST],
      tailParams: Seq[SoftAST.GroupTail[D]]): Option[SoftAST.ExpressionAST] =
    if ((tailParams.nonEmpty && headExpression.isEmpty) || (assertNonEmpty && headExpression.isEmpty && tailParams.isEmpty))
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
  private def tail[Unknown: P, D <: Token](
      delimiter: D,
      expressionsParseOrFail: => P[SoftAST.ExpressionAST]): P[SoftAST.GroupTail[D]] =
    P {
      Index ~
        TokenParser.parseOrFail(delimiter) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expressionsParseOrFail) ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, comma, preParamNameSpace, argumentName, postParamNameSpace, to) =>
        SoftAST.GroupTail(
          index = range(from, to),
          delimiter = comma,
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
        StructConstructorParser.parseOrFail |
        TupleParser.parseOrFail(assertNonEmpty = false) |
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
