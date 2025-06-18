// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST.{ExpressionAST, Space, TokenDocumented}

private object MethodCallParser {

  /** This type is used internally for sequentially parsing dot calls. */
  private case class DotCall(
      index: SourceIndex,
      dot: TokenDocumented[Token.Dot.type],
      postDotSpace: Option[Space],
      rightExpression: ExpressionAST)

  def parseOrFail[Unknown: P]: P[SoftAST.MethodCall] =
    P {
      Index ~
        ExpressionParser.parseSubset(leftExpression) ~
        SpaceParser.parseOrFail.? ~
        dotCall.rep(1)
    } flatMap {
      case (from, leftExpression, preDotSpace, headDotCall :: tailDotCalls) =>
        // Dot calls are sequentially parsed, convert them to `MethodCall` in the order of their execution.
        val headMethodCall =
          SoftAST.MethodCall(
            index = range(from, headDotCall.index.to),
            leftExpression = leftExpression,
            preDotSpace = preDotSpace,
            dot = headDotCall.dot,
            postDotSpace = headDotCall.postDotSpace,
            rightExpression = headDotCall.rightExpression
          )

        val result =
          tailDotCalls.foldLeft(headMethodCall) {
            case (left, dot) =>
              SoftAST.MethodCall(
                index = range(left.index.from, dot.index.to),
                leftExpression = left,
                preDotSpace = preDotSpace,
                dot = dot.dot,
                postDotSpace = dot.postDotSpace,
                rightExpression = dot.rightExpression
              )
          }

        Pass(result)

      case (_, _, _, dotCalls) =>
        // This will never occur because `dotCall.rep(1)` disallows it.
        Fail(s"Expected at least one dot call. Actual: ${dotCalls.size}")
    }

  private def dotCall[Unknown: P]: P[DotCall] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Dot) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(rightExpression) ~
        Index
    } map {
      case (from, dot, postDotSpace, rightExpression, to) =>
        DotCall(
          index = range(from, to),
          dot = dot,
          postDotSpace = postDotSpace,
          rightExpression = rightExpression
        )
    }

  private def leftExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      ReferenceCallParser.parseOrFail |
        ParameterParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        BooleanParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

  private def rightExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      ReferenceCallParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
