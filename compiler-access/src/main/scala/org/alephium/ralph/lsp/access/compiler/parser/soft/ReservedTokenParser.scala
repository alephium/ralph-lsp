// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token

object ReservedTokenParser {

  //  /**
  //   * This code dynamically parses tokens which are much slower than statically defined parser [[reservedToken]].
  //   *
  //   * This code benchmarked on LOC: 1000 yields an average time of:
  //   * 0.1550185278599999997 seconds
  //   *
  //   * Using static tokens yields:
  //   * 0.1087630216499999994  seconds
  //   */
  //  def parseOrFail[Unknown: P]: P[Token.Reserved] =
  //    ParserUtil.orTokenCombinator(
  //      prefixCheck = false,
  //      tokens = Token.reserved.iterator
  //    )

  /**
   * Parses all reserved tokens defined in [[Token.reserved]] and returns the first match.
   *
   * Prefix check is not required here because [[Token.reserved]] contains all tokens and are sorted.
   */
  def parseOrFail[Unknown: P]: P[Token.Reserved] =
    staticReservedToken.! flatMap {
      string =>
        Token.reserved.find(_.lexeme == string) match {
          case Some(token) =>
            Pass(token)

          case None =>
            Fail(s"Unable to find reserved token for string '$string'")
        }
    }

  /**
   * Static string parser is used for reserved tokens because it increases by approximately 30% (29.84%).
   *
   * Test-cases will report if a reserved token ([[Token.Reserved]]) is missing from this list.
   *
   * __Note__: The order is important here so use the following commented out `regenerate` function
   *           to print the tokens to add in order.
   *
   * TODO: This parser should be macro generated for easier management when new tokens are added.
   */
  private def staticReservedToken[Unknown: P]: P[Unit] =
    StringIn(
      """AssetScript""",
      """implements""",
      """Interface""",
      """TxScript""",
      """Contract""",
      """Abstract""",
      """mapping""",
      """extends""",
      """struct""",
      """return""",
      """import""",
      """while""",
      """false""",
      """event""",
      """const""",
      """|**|""",
      """true""",
      """enum""",
      """else""",
      """alph""",
      """ALPH""",
      """|-|""",
      """|+|""",
      """|*|""",
      """pub""",
      """mut""",
      """let""",
      """for""",
      """||""",
      """if""",
      """fn""",
      """>>""",
      """>=""",
      """==""",
      """<=""",
      """<<""",
      """/=""",
      """//""",
      """->""",
      """-=""",
      """+=""",
      """++""",
      """*=""",
      """**""",
      """&&""",
      """!=""",
      """}""",
      """|""",
      """{""",
      """`""",
      """^""",
      """]""",
      """\""",
      """[""",
      """@""",
      """>""",
      """=""",
      """<""",
      """;""",
      """:""",
      """/""",
      """.""",
      """-""",
      """,""",
      """+""",
      """*""",
      """)""",
      """(""",
      """&""",
      """%""",
      """$""",
      """#""",
      """"""",
      """!"""
    )

//  /**
//   * Use this function to print the tokens to paste into the above [[staticReservedToken]].
//   *
//   * TODO: Move this to a macro.
//   */
//  private def regenerate(): Unit = {
//    val string =
//      Token
//        .reserved
//        .map {
//          token =>
//            s"""\"\"\"${token.lexeme}\"\"\""""
//        }
//        .mkString(", ")
//
//    println(string)
//  }

}
