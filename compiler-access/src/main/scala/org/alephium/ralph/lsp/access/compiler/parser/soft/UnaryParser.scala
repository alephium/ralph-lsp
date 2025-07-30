// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object UnaryParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Unary] =
    P {
      Index ~
        TokenParser.parseOrFailOneOf(prefixCheck = true, tokens = Token.unary.iterator) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        Index
    } map {
      case (from, unary, preExpressionSpace, expression, to) =>
        SoftAST.Unary(
          index = range(from, to),
          unaryOperator = unary,
          preExpressionSpace = preExpressionSpace,
          expression = expression
        )
    }

}
