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
        TupleParser.parse ~
        SpaceParser.parseOrFail.? ~
        (BlockParser.parseOrFail.map(Left(_)) | ExpressionParser.parse.map(Right(_))) ~
        // do not read the space if `else` is not provided
        (SpaceParser.parseOrFail.? ~ ElseParser.parseOrFail).? ~
        Index
    } map {
      case (from, ifToken, preGroupSpace, group, preBodySpace, block, elseStatement, to) =>
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
          preBodySpace = preBodySpace,
          body = block,
          preElseSpace = elseSpace,
          elseStatement = elseAST
        )
    }

  /** Parses the body of an `if` or `else` expression */
  def parseBody[Unknown: P]: P[Either[SoftAST.Block, SoftAST.ExpressionAST]] =
    P(BlockParser.parseOrFail.map(Left(_)) | ExpressionParser.parse.map(Right(_)))

}
