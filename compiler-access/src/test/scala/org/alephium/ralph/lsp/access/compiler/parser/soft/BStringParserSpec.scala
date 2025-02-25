package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse.assertIsFastParseError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class BStringParserSpec extends AnyWordSpec with Matchers {

  "b alone" should {
    "not parse as String literal" in {
      // this is because "b" is not a reserved token.
      // "b" followed by tick is required for string literal.
      assertIsFastParseError {
        parseBString("b")
      }
    }
  }

  "b followed by a tick" should {
    "parse as string literal" in {
      // this is successfully parsed and the ending tick is reported as missing.
      val string =
        parseBString("b`") // closing tick is missing

      string shouldBe
        SoftAST.BString(
          index = indexOf(">>b`<<"),
          b = B(">>b<<`"),
          postBSpace = None,
          startTick = Tick("b>>`<<"),
          text = None,
          endTick = SoftAST.TokenExpected(indexOf("b`>><<"), Token.Tick)
        )
    }
  }

  "empty string" in {
    val string =
      parseBString("b``")

    string shouldBe
      SoftAST.BString(
        index = indexOf(">>b``<<"),
        b = B(">>b<<``"),
        postBSpace = None,
        startTick = Tick("b>>`<<`"),
        text = None,
        endTick = Tick("b`>>`<<")
      )
  }

  "spaces only" in {
    val string =
      parseBString("b ` `")

    string shouldBe
      SoftAST.BString(
        index = indexOf(">>b ` `<<"),
        b = B(">>b<< ` `"),
        postBSpace = Some(Space("b>> <<` `")),
        startTick = Tick("b >>`<< `"),
        text = Some(SoftAST.CodeString(indexOf("b `>> <<`"), " ")),
        endTick = Tick("b ` >>`<<")
      )
  }

  "comment exist before b" in {
    val string =
      parseBString {
        """// this is a byte string
          |b ` `""".stripMargin
      }

    // Simply check that the comment exists before b.
    // No need to assert the comment AST, CommentsSpec should assert this.
    val comments =
      string.b.documentation.value

    comments.index shouldBe
      indexOf {
        """>>// this is a byte string
          |<<b ` `""".stripMargin
      }

    comments.toCode() shouldBe
      """// this is a byte string
        |""".stripMargin

  }

  "comment exist before tick" in {
    val string =
      parseBString {
        """b
          |// this is a byte string
          |` `""".stripMargin
      }

    // Check that the comment exists before b.
    // No need to assert the comment AST, CommentsSpec should assert this.
    val comments =
      string.startTick.documentation.value

    comments.index shouldBe
      indexOf {
        """b
          |>>// this is a byte string
          |<<` `""".stripMargin
      }

    comments.toCode() shouldBe
      """// this is a byte string
        |""".stripMargin

  }

  "string characters exist" when {
    "closing tick is missing" should {
      "parse the entire string" in {
        val string =
          parseBString {
            """b` this is
              |
              |a string value
              |""".stripMargin
          }

        string.text.value shouldBe
          SoftAST.CodeString(
            index = indexOf {
              """b`>> this is
                |
                |a string value
                |<<""".stripMargin
            },
            text =
              """ this is
                |
                |a string value
                |""".stripMargin
          )

        string.text.value.text shouldBe
          """ this is
            |
            |a string value
            |""".stripMargin

        string.endTick shouldBe
          SoftAST.TokenExpected(
            index = indexOf {
              """b` this is
                |
                |a string value
                |>><<""".stripMargin
            },
            token = Token.Tick
          )
      }
    }
  }

}
