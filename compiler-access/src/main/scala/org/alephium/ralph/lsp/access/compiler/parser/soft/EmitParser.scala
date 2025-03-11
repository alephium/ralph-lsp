// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object EmitParser {

  /**
   * Syntax:
   *
   * {{{
   *   emit expression
   * }}}
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Emit] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Emit) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parseSubset(expression) ~
        Index
    } map {
      case (from, emit, space, expression, to) =>
        SoftAST.Emit(
          index = range(from, to),
          emit = emit,
          preExpressionSpace = space,
          expression = expression
        )
    }

  private def expression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      MethodCallParser.parseOrFail |
        BlockParser.parseOrFail |
        IfElseParser.parseOrFail |
        ReferenceCallParser.parseOrFail |
        IdentifierParser.parseOrFail
    }

}
