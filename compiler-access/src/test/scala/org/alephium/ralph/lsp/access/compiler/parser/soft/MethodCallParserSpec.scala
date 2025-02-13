package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MethodCallParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "dot is not provided" when {
      "parsing directly" in {
        assertIsFastParseError {
          parseMethodCall("abc")
        }
      }

      "parsing via SoftParser" in {
        // It's not a method call, it's an identifier.
        val body =
          parseSoft("abc")

        body.parts should have size 1
        body.parts.head.part shouldBe
          Identifier(
            index = indexOf(">>abc<<"),
            text = "abc"
          )
      }
    }
  }

  "pass" when {
    "dot is provided" when {
      "no expressions" in {
        val dot = parseMethodCall(".")

        dot shouldBe
          SoftAST.MethodCall(
            index = indexOf(">>.<<"),
            leftExpression = SoftAST.ExpressionExpected(indexOf(">><<.")),
            preDotSpace = None,
            dotCalls = Seq(
              SoftAST.DotCall(
                index = indexOf(">>.<<"),
                dot = Dot(indexOf(">>.<<")),
                postDotSpace = None,
                rightExpression = SoftAST.ExpressionExpected(indexOf(".>><<"))
              )
            )
          )
      }

      "no left expression" in {
        val dot = parseMethodCall(".right")

        dot shouldBe
          SoftAST.MethodCall(
            index = indexOf(">>.right<<"),
            leftExpression = SoftAST.ExpressionExpected(indexOf(">><<.right")),
            preDotSpace = None,
            dotCalls = Seq(
              SoftAST.DotCall(
                index = indexOf(">>.right<<"),
                dot = Dot(indexOf(">>.<<right")),
                postDotSpace = None,
                rightExpression = Identifier(indexOf(".>>right<<"), "right")
              )
            )
          )
      }

      "no right expression" in {
        val dot = parseMethodCall("left.")

        dot shouldBe
          SoftAST.MethodCall(
            index = indexOf(">>left.<<"),
            leftExpression = Identifier(indexOf(">>left<<."), "left"),
            preDotSpace = None,
            dotCalls = Seq(
              SoftAST.DotCall(
                index = indexOf("left>>.<<"),
                dot = Dot(indexOf("left>>.<<")),
                postDotSpace = None,
                rightExpression = SoftAST.ExpressionExpected(indexOf("left.>><<"))
              )
            )
          )
      }

      "with expressions" in {
        val dot = parseMethodCall("left.right")

        dot shouldBe
          SoftAST.MethodCall(
            index = indexOf(">>left.right<<"),
            leftExpression = Identifier(indexOf(">>left<<.right"), "left"),
            preDotSpace = None,
            dotCalls = Seq(
              SoftAST.DotCall(
                index = indexOf("left>>.right<<"),
                dot = Dot(indexOf("left>>.<<right")),
                postDotSpace = None,
                rightExpression = Identifier(indexOf("left.>>right<<"), "right")
              )
            )
          )
      }
    }
  }

  "decimal number" should {
    "not be parsed as a method call" when {
      "called directly" in {
        assertIsFastParseError {
          parseMethodCall("1.1")
        }
      }

      "called via soft-parser" in {
        val number = parseSoft("1.1")
        number.parts should have size 1
        val head = number.parts.head.part

        head shouldBe
          SoftAST.Number(
            index = indexOf(">>1.1<<"),
            documentation = None,
            number = SoftAST.CodeString(
              index = indexOf(">>1.1<<"),
              text = "1.1"
            ),
            unit = None
          )
      }
    }
  }

}
