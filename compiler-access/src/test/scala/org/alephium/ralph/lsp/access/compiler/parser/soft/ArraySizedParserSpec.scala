// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArraySizedParserSpec extends AnyWordSpec with Matchers {

  "parse an empty array" when {
    "no elements are provided" in {
      val ast =
        parseArray("[;]")

      ast shouldBe
        SoftAST.ArraySized(
          index = indexOf(">>[;]<<"),
          openBracket = OpenBracket(">>[<<;]"),
          preTypeSpace = None,
          tpe = IdentifierExpected("[>><<;]"),
          preSemiColonSpace = None,
          semiColon = Semicolon("[>>;<<]"),
          preSizeSpace = None,
          size = ExpressionExpected("[;>><<]"),
          preCloseBracketSpace = None,
          closeBracket = BlockBracket("[;>>]<<")
        )
    }

    "no elements are provided and the closing bracket is missing" in {
      val ast =
        parseArray("[;")

      ast shouldBe
        SoftAST.ArraySized(
          index = indexOf(">>[;<<"),
          openBracket = OpenBracket(">>[<<;"),
          preTypeSpace = None,
          tpe = IdentifierExpected("[>><<;"),
          preSemiColonSpace = None,
          semiColon = Semicolon("[>>;<<"),
          preSizeSpace = None,
          size = ExpressionExpected("[;>><<"),
          preCloseBracketSpace = None,
          closeBracket = TokenExpected("[;>><<", Token.BlockBracket)
        )
    }
  }

  /**
   * **************************
   * Non-empty array test-cases
   * **************************
   */
  "sized array is fully defined" in {
    val ast =
      parseArray("[Type; Size]")

    ast shouldBe
      SoftAST.ArraySized(
        index = indexOf(">>[Type; Size]<<"),
        openBracket = OpenBracket(">>[<<Type; Size]"),
        preTypeSpace = None,
        tpe = Identifier("[>>Type<<; Size]"),
        preSemiColonSpace = None,
        semiColon = Semicolon("[Type>>;<< Size]"),
        preSizeSpace = Some(Space("[Type;>> <<Size]")),
        size = Identifier("[Type; >>Size<<]"),
        preCloseBracketSpace = None,
        closeBracket = BlockBracket("[Type; Size>>]<<")
      )
  }

}
