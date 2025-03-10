// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object IfElseParser {

  def parseOrFail[Unknown: P]: P[SoftAST.IfElse] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.If) ~
        TokenParser.isBoundary(Token.OpenParen, Token.OpenCurly) ~
        SpaceParser.parseOrFail.? ~
        ParameterParser.parse ~
        SpaceParser.parseOrFail.? ~
        BlockParser.parseOrFail.? ~
        // do not read the space if `else` is not provided
        (SpaceParser.parseOrFail.? ~ ElseParser.parseOrFail).? ~
        Index
    } map {
      case (from, ifToken, preGroupSpace, group, preBlockSpace, block, elseStatement, to) =>
        val (elseSpace, elseAST) =
          elseStatement match {
            case Some((space, statement)) =>
              (space, Some(statement))

            case None =>
              (None, None)
          }

        SoftAST.IfElse(
          index = range(from, to),
          ifToken = ifToken,
          preGroupSpace = preGroupSpace,
          group = group,
          preBlockSpace = preBlockSpace,
          block = block,
          preElseSpace = elseSpace,
          elseStatement = elseAST
        )
    }

}
