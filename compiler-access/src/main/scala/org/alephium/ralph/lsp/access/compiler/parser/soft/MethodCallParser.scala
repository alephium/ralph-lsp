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
      preRightExpressionSpace: Option[Space],
      rightExpression: ExpressionAST,
      preAssetApprovalSpace: Option[Space],
      assetApproval: Option[SoftAST.AssetApproval])

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
            postDotSpace = headDotCall.preRightExpressionSpace,
            rightExpression = headDotCall.rightExpression,
            preAssetApprovalSpace = headDotCall.preAssetApprovalSpace,
            assetApproval = headDotCall.assetApproval
          )

        val result =
          tailDotCalls.foldLeft(headMethodCall) {
            case (left, dot) =>
              SoftAST.MethodCall(
                index = range(left.index.from, dot.index.to),
                leftExpression = left,
                preDotSpace = preDotSpace,
                dot = dot.dot,
                postDotSpace = dot.preRightExpressionSpace,
                rightExpression = dot.rightExpression,
                preAssetApprovalSpace = dot.preAssetApprovalSpace,
                assetApproval = dot.assetApproval
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
        (SpaceParser.parseOrFail.? ~ AssetApprovalParser.parseOrFail).? ~
        Index
    } map {
      case (from, dot, preRightExpressionSpace, rightExpression, assetApproval, to) =>
        DotCall(
          index = range(from, to),
          dot = dot,
          preRightExpressionSpace = preRightExpressionSpace,
          rightExpression = rightExpression,
          preAssetApprovalSpace = assetApproval.flatMap(_._1),
          assetApproval = assetApproval.map(_._2)
        )
    }

  private def leftExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      ReferenceCallParser.parseOrFail |
        TupleParser.parseOrFail(assertNonEmpty = false) |
        ArrayParser.parseOrFail |
        ByteVecParser.parseOrFail |
        NumberParser.parseOrFail |
        UnaryParser.parseOrFail |
        BooleanParser.parseOrFail |
        AlphParser.parseOrFail |
        BStringParser.parseOrFail |
        StringInterpolationParser.parseOrFail |
        ArrayAccessParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

  private def rightExpression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      ReferenceCallParser.parseOrFail |
        ArrayAccessParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
