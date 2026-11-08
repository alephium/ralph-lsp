// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TokenParsersSpec extends AnyWordSpec with Matchers {

  private def reserved[Unknown: P]: P[Token.Reserved]   = TokenParsers.parse(Token.reserved.toSeq, false)
  private def infix[Unknown: P]: P[Token.InfixOperator] = TokenParsers.parse(Token.infix.toSeq, false)
  private def unary[Unknown: P]: P[Token.Unary]         = TokenParsers.parse(Token.unary.toSeq, true)
  private def reversed[Unknown: P]: P[Token.Reserved]   = TokenParsers.parse(Token.reserved.reverse.toSeq, false)
  private def singleton[Unknown: P]: P[Token.Plus.type] = TokenParsers.parse(Seq(Token.Plus), true)
  private def unguarded[Unknown: P]: P[Token.Plus.type] = TokenParsers.parse(Seq(Token.Plus), false)
  private def spaces[Unknown: P]: P[Token.Space]        = TokenParsers.parse(Token.spaces, false)

  "static token parsers" should {
    "return the exact typed singleton for every reserved and infix token" in {
      Token.reserved.foreach {
        token =>
          parse(token.lexeme, reserved(_)) shouldBe Parsed.Success(token, token.lexeme.length)
          parse(token.lexeme, reversed(_)) shouldBe Parsed.Success(token, token.lexeme.length)
      }
      Token.infix.foreach {
        token =>
          parse(token.lexeme, infix(_)) shouldBe Parsed.Success(token, token.lexeme.length)
      }
    }
    "prefer the longest operator regardless of input collection order" in {
      Seq("|**|", "||", "**", ">=", ">>", "+=", "++", "//", "->").foreach {
        text =>
          val expected = Token.reserved.find(_.lexeme == text).get
          parse(text + "tail", reversed(_)) shouldBe Parsed.Success(expected, text.length)
      }
    }
    "preserve unary prefix exclusions and singleton result types" in {
      Seq("++", "+=", "-=", "->", "!=").foreach {
        text =>
          parse(text, unary(_)) shouldBe a[Parsed.Failure]
      }
      parse("+name", singleton(_)) shouldBe Parsed.Success(Token.Plus, 1)
      parse("!name", unary(_)) shouldBe Parsed.Success(Token.Exclamation, 1)
      parse("++", singleton(_)) shouldBe a[Parsed.Failure]
      parse("++", unguarded(_)) shouldBe Parsed.Success(Token.Plus, 1)
    }
    "fail without consuming unrelated input" in {
      parse("unknown", reserved(_)) shouldBe a[Parsed.Failure]
      parse("", infix(_)) shouldBe a[Parsed.Failure]
    }
    "use the runtime platform newline" in {
      Token.spaces.foreach {
        token =>
          parse(token.lexeme, spaces(_)) shouldBe Parsed.Success(token, token.lexeme.length)
      }
    }
    "reject invalid compile-time token collections" in {
      assertDoesNotCompile("""def invalid[Unknown: fastparse.P] = TokenParsers.parse(Seq.empty[Token.Reserved], false)""")
      assertDoesNotCompile("""def invalid[Unknown: fastparse.P] = TokenParsers.parse(Seq(Token.Plus, Token.Plus), false)""")
      assertDoesNotCompile("""def invalid[Unknown: fastparse.P]: fastparse.P[Token.Plus.type] = TokenParsers.parse(Seq(Token.Minus), false)""")
      assertDoesNotCompile("""def invalid[Unknown: fastparse.P](tokens: Seq[Token]) = TokenParsers.parse(tokens, false)""")
    }
  }

}
