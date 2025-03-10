// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object ConstParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Const] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Const) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        AssignmentParser.parse ~
        Index
    } map {
      case (from, const, preAssignmentSpace, assignment, to) =>
        SoftAST.Const(
          index = range(from, to),
          constToken = const,
          preAssignmentSpace = preAssignmentSpace,
          assignment = assignment
        )
    }

}
