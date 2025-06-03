// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
        val root =
          parseSoft("abc")

        root.parts should have size 1
        root.parts.head shouldBe
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
        val method = parseMethodCall(".")

        method shouldBe
          SoftAST.MethodCall(
            index = indexOf(">>.<<"),
            leftExpression = ExpressionExpected(">><<."),
            preDotSpace = None,
            dot = Dot(">>.<<"),
            postDotSpace = None,
            rightExpression = ExpressionExpected(".>><<")
          )
      }

      "no left expression" in {
        val method = parseMethodCall(".right")

        method shouldBe
          SoftAST.MethodCall(
            index = indexOf(">>.right<<"),
            leftExpression = ExpressionExpected(">><<.right"),
            preDotSpace = None,
            dot = Dot(">>.<<right"),
            postDotSpace = None,
            rightExpression = Identifier(".>>right<<")
          )
      }

      "no right expression" in {
        val method = parseMethodCall("left.")

        method shouldBe
          SoftAST.MethodCall(
            index = indexOf(">>left.<<"),
            leftExpression = Identifier(">>left<<."),
            preDotSpace = None,
            dot = Dot("left>>.<<"),
            postDotSpace = None,
            rightExpression = ExpressionExpected("left.>><<")
          )
      }

      "with expressions" in {
        val method = parseMethodCall("left.right")

        method shouldBe
          SoftAST.MethodCall(
            index = indexOf(">>left.right<<"),
            leftExpression = Identifier(">>left<<.right"),
            preDotSpace = None,
            dot = Dot("left>>.<<right"),
            postDotSpace = None,
            rightExpression = Identifier("left.>>right<<")
          )
      }
    }

    "three method calls" in {
      val actual = parseMethodCall("a.b.c")

      val expected =
        SoftAST.MethodCall(
          index = indexOf(">>a.b.c<<"),
          leftExpression = SoftAST.MethodCall(
            index = indexOf(">>a.b<<.c"),
            leftExpression = Identifier(">>a<<.b.c"),
            preDotSpace = None,
            dot = Dot("a>>.<<b.c"),
            postDotSpace = None,
            rightExpression = Identifier("a.>>b<<.c")
          ),
          preDotSpace = None,
          dot = Dot("a.b>>.<<c"),
          postDotSpace = None,
          rightExpression = Identifier("a.b.>>c<<")
        )

      actual shouldBe expected
    }

    "four method calls" in {
      val actual = parseMethodCall("a.b.c.d")

      val expected =
        SoftAST.MethodCall(
          index = indexOf(">>a.b.c.d<<"),
          leftExpression = SoftAST.MethodCall(
            index = indexOf(">>a.b.c<<.d"),
            leftExpression = SoftAST.MethodCall(
              index = indexOf(">>a.b<<.c.d"),
              leftExpression = Identifier(">>a<<.b.c.d"),
              preDotSpace = None,
              dot = Dot("a>>.<<b.c.d"),
              postDotSpace = None,
              rightExpression = Identifier("a.>>b<<.c.d")
            ),
            preDotSpace = None,
            dot = Dot("a.b>>.<<c.d"),
            postDotSpace = None,
            rightExpression = Identifier("a.b.>>c<<.d")
          ),
          preDotSpace = None,
          dot = Dot("a.b.c>>.<<d"),
          postDotSpace = None,
          rightExpression = Identifier("a.b.c.>>d<<")
        )

      actual shouldBe expected
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
        val head = number.parts.head

        head shouldBe
          SoftAST.Number(
            index = indexOf(">>1.1<<"),
            documentation = None,
            number = CodeString(">>1.1<<"),
            unit = None
          )
      }
    }
  }

}
