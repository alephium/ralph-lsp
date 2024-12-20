package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.CommonParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

case object MethodCallParser {

  def parseOrFail[Unknown: P]: P[SoftAST.MethodCall] =
    P {
      Index ~
        ExpressionParser.parseOrFailSelective(parseInfix = false, parseMethodCall = false) ~
        spaceOrFail.? ~
        dotCall.rep(1) ~
        Index
    } map {
      case (from, leftExpression, preDotSpace, dotCalls, to) =>
        SoftAST.MethodCall(
          index = range(from, to),
          leftExpression = leftExpression,
          preDotSpace = preDotSpace,
          dotCalls = dotCalls
        )
    }

  private def dotCall[Unknown: P]: P[SoftAST.DotCall] =
    P {
      Index ~
        TokenParser.DotOrFail ~
        spaceOrFail.? ~
        ReferenceCallParser.parse ~
        Index
    } map {
      case (from, dot, postDotSpace, rightExpression, to) =>
        SoftAST.DotCall(
          index = range(from, to),
          dot = dot,
          postDotSpace = postDotSpace,
          rightExpression = rightExpression
        )
    }

}
