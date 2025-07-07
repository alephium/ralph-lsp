// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object BlockPartParser {

  def parseOrFail[Unknown: P](
      blockExpressions: Boolean,
      stop: Token*): P[SoftAST.BlockPartAST] =
    P {
      SpaceParser.parseOrFail |
        TemplateParser.parseOrFail |
        EventParser.parseOrFail |
        StructParser.parseOrFail |
        ConstParser.parseOrFail |
        FunctionParser.parseOrFail |
        EnumParser.parseOrFail |
        ImportParser.parseOrFail |
        InheritanceParser.parseOrFail |
        expression(blockExpressions) |
        CommentParser.parseOrFail |
        UnresolvedParser.parseOrFailSpaceDelimited(stop)
    }

  private def expression[Unknown: P](blockExpressions: Boolean): P[SoftAST.BlockPartAST] =
    if (blockExpressions)
      ExpressionBlockParser.parseOrFail
    else
      ExpressionParser.parseOrFail

}
