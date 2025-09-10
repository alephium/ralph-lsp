// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object InfixTokenParser {

  /**
   * Parses all tokens of type [[Token.InfixOperator]] and also their comments.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.TokenDocumented[Token.InfixOperator]] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        Index ~
        infixToken ~
        Index
    } map {
      case (commentFrom, documentation, tokenFrom, token, to) =>
        SoftAST.TokenDocumented(
          index = range(commentFrom, to),
          documentation = documentation,
          code = SoftAST.CodeToken(
            index = range(tokenFrom, to),
            token = token
          )
        )
    }

  /**
   * Parses all infix tokens defined in [[Token.infix]] and returns the first match.
   *
   * Prefix check is not required here because [[Token.infix]] contains all infix tokens and are sorted.
   */
  private def infixToken[Unknown: P]: P[Token.InfixOperator] =
    staticTokens.! flatMap {
      string =>
        Token.infix.find(_.lexeme == string) match {
          case Some(token) =>
            Pass(token)

          case None =>
            Fail(s"Unable to find infix token for string '$string'")
        }
    }

  /**
   * Static string parser is used for reserved tokens because it's faster.
   *
   * Test-cases will report if a token ([[Token.InfixOperator]]) is missing from this list.
   *
   * __Note__: The order is important here so use the following commented out `regenerate` function
   *           to print the tokens to add in order.
   *
   * TODO: This parser should be macro generated for easier management when new tokens are added.
   */
  private def staticTokens[Unknown: P]: P[Unit] =
    StringIn(
      """|**|""",
      """|-|""",
      """|+|""",
      """|*|""",
      """||""",
      """>>""",
      """>=""",
      """==""",
      """<=""",
      """<<""",
      """/=""",
      """-=""",
      """+=""",
      """++""",
      """*=""",
      """**""",
      """&&""",
      """!=""",
      """|""",
      """^""",
      """\""",
      """>""",
      """<""",
      """/""",
      """-""",
      """+""",
      """*""",
      """&""",
      """%"""
    )

}
