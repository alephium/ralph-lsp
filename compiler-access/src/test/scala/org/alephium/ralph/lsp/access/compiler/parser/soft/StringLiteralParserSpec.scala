// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse.assertIsFastParseError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class StringLiteralParserSpec extends AnyWordSpec with Matchers {

  private val newline =
    Token.Newline.lexeme

  "fail" when {
    "empty" in {
      assertIsFastParseError {
        parseStringLiteral("")
      }
    }

    "blank" in {
      assertIsFastParseError {
        parseStringLiteral("  ")
      }
    }

    "not a quote" in {
      assertIsFastParseError {
        parseStringLiteral("b")
      }
    }
  }

  "succeed as a regular String (not a path)" when {
    "closing quote is missing" when {
      "text is empty" in {
        val string =
          parseStringLiteral("\"")

        string shouldBe
          SoftAST.StringLiteral(
            index = indexOf(">>\"<<"),
            startQuote = Quote(">>\"<<"),
            head = None,
            tail = Seq.empty,
            endQuote = SoftAST.TokenExpected(indexOf("\">><<"), Token.Quote)
          )
      }

      "text is non-empty" in {
        val string =
          parseStringLiteral("\" a b c ")

        string shouldBe
          SoftAST.StringLiteral(
            index = indexOf(">>\" a b c <<"),
            startQuote = Quote(">>\"<< a b c "),
            head = Some(SoftAST.CodeString(indexOf("\">> a b c <<"), " a b c ")),
            tail = Seq.empty,
            endQuote = SoftAST.TokenExpected(indexOf("\" a b c >><<"), Token.Quote)
          )
      }

      "text is non-empty with newline" in {
        val string =
          parseStringLiteral(s"\" $newline a b c $newline ")

        string shouldBe
          SoftAST.StringLiteral(
            index = indexOf(s">>\" $newline a b c $newline <<"),
            startQuote = Quote(s">>\"<< $newline a b c $newline "),
            head = Some(SoftAST.CodeString(indexOf(s"\">> $newline a b c $newline <<"), s" $newline a b c $newline ")),
            tail = Seq.empty,
            endQuote = SoftAST.TokenExpected(indexOf(s"\" $newline a b c $newline >><<"), Token.Quote)
          )
      }
    }

    "closing quote is provided" when {
      "text is non-empty with newlines" in {
        val string =
          parseStringLiteral(s"\" $newline a b c $newline \"")

        string shouldBe
          SoftAST.StringLiteral(
            index = indexOf(s">>\" $newline a b c $newline \"<<"),
            startQuote = Quote(s">>\"<< $newline a b c $newline \""),
            head = Some(
              SoftAST.CodeString(
                index = indexOf(s"\">> $newline a b c $newline <<\""),
                text = s" $newline a b c $newline "
              )
            ),
            tail = Seq.empty,
            endQuote = Quote(s"\" $newline a b c $newline >>\"<<")
          )
      }
    }
  }

  "succeed as a path String (contains forward slashes defining a path)" when {
    "closing quote is missing" when {
      "head path is empty" in {
        val string =
          parseStringLiteral("\"/b")

        string shouldBe
          SoftAST.StringLiteral(
            index = indexOf(">>\"/b<<"),
            startQuote = Quote(">>\"<</b"),
            head = Some(SoftAST.CodeStringExpected(indexOf("\">><</b"))),
            tail = Seq(
              SoftAST.Path(
                index = indexOf("\">>/b<<"),
                slash = ForwardSlash("\">>/<<b"),
                text = SoftAST.CodeString(indexOf("\"/>>b<<"), "b")
              )
            ),
            endQuote = SoftAST.TokenExpected(indexOf("\"/b>><<"), Token.Quote)
          )
      }

      "tail path is empty" in {
        val string =
          parseStringLiteral("\"a/")

        string shouldBe
          SoftAST.StringLiteral(
            index = indexOf(">>\"a/<<"),
            startQuote = Quote(">>\"<<a/"),
            head = Some(SoftAST.CodeString(indexOf("\">>a<</"), "a")),
            tail = Seq(
              SoftAST.Path(
                index = indexOf("\"a>>/<<"),
                slash = ForwardSlash("\"a>>/<<"),
                text = SoftAST.CodeStringExpected(indexOf("\"a/>><<"))
              )
            ),
            endQuote = SoftAST.TokenExpected(indexOf("\"a/>><<"), Token.Quote)
          )
      }

      "tail path non-empty" in {
        val string =
          parseStringLiteral("\" a / b / c ")

        string shouldBe
          SoftAST.StringLiteral(
            index = indexOf(">>\" a / b / c <<"),
            startQuote = Quote(">>\"<< a / b / c "),
            head = Some(SoftAST.CodeString(indexOf("\">> a <</ b / c "), " a ")),
            tail = Seq(
              SoftAST.Path(
                index = indexOf("\" a >>/ b <</ c "),
                slash = ForwardSlash("\" a >>/<< b / c "),
                text = SoftAST.CodeString(indexOf("\" a />> b <</ c "), " b ")
              ),
              SoftAST.Path(
                index = indexOf("\" a / b >>/ c <<"),
                slash = ForwardSlash("\" a / b >>/<< c "),
                text = SoftAST.CodeString(indexOf("\" a / b />> c <<"), " c ")
              )
            ),
            endQuote = SoftAST.TokenExpected(indexOf("\" a / b / c >><<"), Token.Quote)
          )
      }
    }

    "closing quote is provided" when {
      "text is non-empty with newlines" when {
        "it is not a path but a regular String literal" in {
          val string =
            parseStringLiteral(s"\" $newline a b c $newline \"")

          string shouldBe
            SoftAST.StringLiteral(
              index = indexOf(s">>\" $newline a b c $newline \"<<"),
              startQuote = Quote(s">>\"<< $newline a b c $newline \""),
              head = Some(
                SoftAST.CodeString(
                  index = indexOf(s"\">> $newline a b c $newline <<\""),
                  text = s" $newline a b c $newline "
                )
              ),
              tail = Seq.empty,
              endQuote = Quote(s"\" $newline a b c $newline >>\"<<")
            )
        }

        "it is a path" in {
          val string =
            parseStringLiteral("\" a / b / c \"")

          string shouldBe
            SoftAST.StringLiteral(
              index = indexOf(">>\" a / b / c \"<<"),
              startQuote = Quote(">>\"<< a / b / c \""),
              head = Some(SoftAST.CodeString(indexOf("\">> a <</ b / c \""), " a ")),
              tail = Seq(
                SoftAST.Path(
                  index = indexOf("\" a >>/ b <</ c \""),
                  slash = ForwardSlash("\" a >>/<< b / c \""),
                  text = SoftAST.CodeString(indexOf("\" a />> b <</ c \""), " b ")
                ),
                SoftAST.Path(
                  index = indexOf("\" a / b >>/ c <<\""),
                  slash = ForwardSlash("\" a / b >>/<< c \""),
                  text = SoftAST.CodeString(indexOf("\" a / b />> c <<\""), " c ")
                )
              ),
              endQuote = Quote("\" a / b / c >>\"<<")
            )
        }
      }
    }
  }

  "SoftParser" should {
    "parse StringLiteral" in {
      val string =
        parseSoft {
          """
            |fn function() -> {}
            |
            |// Some comment
            |"some string"
            |
            |""".stripMargin
        }

      val parts = string.partsNonEmpty
      parts should have size 2

      // first is a function
      parts.head shouldBe a[SoftAST.Function]

      // second is a string-literal
      val stringLit = parts.last.asInstanceOf[SoftAST.StringLiteral]
      stringLit.startQuote.documentation.value.toCode() shouldBe s"// Some comment$newline"
      stringLit.head.value.asInstanceOf[SoftAST.CodeString].text shouldBe "some string"
    }
  }

}
