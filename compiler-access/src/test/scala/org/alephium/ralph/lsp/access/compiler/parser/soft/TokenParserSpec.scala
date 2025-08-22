// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TokenParserSpec extends AnyWordSpec {

  "InfixOperatorOrFail" when {
    "ForwardSlash" should {
      "not parse double forward slash as it is reserved for comments" in {
        assertThrows[Exception](
          parseInfixOperatorOrFail("//")
        )
      }
    }

    "all infix operators" should {
      "succeed" in {
        val infixOperators = Token.infix
        infixOperators should not be empty

        infixOperators foreach {
          infix =>
            parseInfixOperatorOrFail(infix.lexeme) shouldBe
              TokenDocumented(
                index = range(0, infix.lexeme.length),
                token = infix
              )
        }
      }
    }
  }

  "otherReservedTokenWithThisPrefix" should {
    "fetch other reserved tokens with same prefixes" when {
      "-" in {
        Token.Minus.otherReservedTokensWithThisPrefix should contain only (Token.MinusEquals, Token.ForwardArrow)
      }

      "+" in {
        Token.Plus.otherReservedTokensWithThisPrefix should contain only (Token.PlusPlus, Token.PlusEquals)
      }

      "++" in {
        Token.PlusEquals.otherReservedTokensWithThisPrefix shouldBe empty
      }

      "/" in {
        Token.ForwardSlash.otherReservedTokensWithThisPrefix should contain only (Token.DoubleForwardSlash, Token.DivideEquals)
      }

      "//" in {
        Token.DoubleForwardSlash.otherReservedTokensWithThisPrefix shouldBe empty
      }
    }
  }

  "parseOrFailOneOf" when {
    "multiple tokens with same prefixes" should {
      "parse the one with exact match" in {
        val tokens =
          List(
            Token.Plus,
            Token.PlusPlus,
            Token.PlusEquals
          )

        val expected =
          PlusEquals(indexOf(">>+=<<"))

        tokens.permutations foreach {
          _ =>
            val actual = parseOrFailOneOf(prefixCheck = true, tokens: _*)("+=")
            actual shouldBe expected
        }
      }
    }
  }

}
