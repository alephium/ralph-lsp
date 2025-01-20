package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private case object InfixCallParser {

  def parseOrFail[Unknown: P]: P[SoftAST.InfixExpression] =
    P {
      Index ~
        ExpressionParser.parseOrFailSelective(parseInfix = false, parseMethodCall = true, parseAssignment = false) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.InfixOperatorOrFail ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, leftExpression, preOperatorSpace, operator, postOperatorSpace, rightExpression, to) =>
        SoftAST.InfixExpression(
          index = range(from, to),
          leftExpression = leftExpression,
          preOperatorSpace = preOperatorSpace,
          operator = operator,
          postOperatorSpace = postOperatorSpace,
          rightExpression = rightExpression
        )
    }

}
