// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class IfElseParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "`if` is not a keyword" in {
      assertIsFastParseError {
        parseConst("iffy")
      }
    }
  }

  "pass" when {
    "only `if` is defined" in {
      val ifElse = parseIfElse("if")

      ifElse shouldBe
        SoftAST.IfElse(
          index = indexOf(">>if<<"),
          ifToken = If(">>if<<"),
          preGroupSpace = None,
          group = SoftAST.Group(
            index = indexOf("if>><<"),
            openToken = Some(TokenExpected("if>><<", Token.OpenParen)),
            preHeadExpressionSpace = None,
            headExpression = None,
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(TokenExpected("if>><<", Token.CloseParen))
          ),
          preBlockSpace = None,
          block = None,
          preElseSpace = None,
          elseStatement = None
        )
    }

    "`if` condition is defined" in {
      val ifElse = parseIfElse("if(a + b == 1)")

      ifElse.copy(group = null) shouldBe
        SoftAST.IfElse(
          index = indexOf(">>if(a + b == 1)<<"),
          ifToken = If(">>if<<(a + b == 1)"),
          preGroupSpace = None,
          group = null,
          preBlockSpace = None,
          block = None,
          preElseSpace = None,
          elseStatement = None
        )

      ifElse.group.toCode() shouldBe "(a + b == 1)"
    }

    "`if` is fully defined" in {
      val ifElse = parseIfElse("if(a + b == 1) { }")

      ifElse.copy(group = null, block = null) shouldBe
        SoftAST.IfElse(
          index = indexOf(">>if(a + b == 1) { }<<"),
          ifToken = If(">>if<<(a + b == 1) { }"),
          preGroupSpace = None,
          group = null,
          preBlockSpace = Some(Space("if(a + b == 1)>> <<{ }")),
          block = null,
          preElseSpace = None,
          elseStatement = None
        )

      ifElse.group.toCode() shouldBe "(a + b == 1)"
      ifElse.block.value.toCode() shouldBe "{ }"
    }

    "`if` and `else` are both defined" in {
      val ifElse = parseIfElse("if(a + b == 1) { } else { }")

      ifElse.copy(group = null, block = null, elseStatement = null) shouldBe
        SoftAST.IfElse(
          index = indexOf(">>if(a + b == 1) { } else { }<<"),
          ifToken = If(">>if<<(a + b == 1) { } else { }"),
          preGroupSpace = None,
          group = null,
          preBlockSpace = Some(Space("if(a + b == 1)>> <<{ } else { }")),
          block = null,
          preElseSpace = Some(Space("if(a + b == 1) { }>> <<else { }")),
          elseStatement = null
        )

      ifElse.group.toCode() shouldBe "(a + b == 1)"
      ifElse.block.value.toCode() shouldBe "{ }"
      ifElse.elseStatement.value.toCode() shouldBe "else { }"
    }

    "not read the tail `if` space if `else` is not defined" in {
      val root = parseSoft("if (a + b == 1) { } ")

      // Head part is the `if` statement.
      // Last part is the space, not read as part of the `if` statement.
      root.parts should have size 2

      // Expect that the tail space is not read by the `if` parser.
      val iff = root.parts.head.asInstanceOf[SoftAST.IfElse]
      iff.toCode() shouldBe "if (a + b == 1) { }"
      iff.index shouldBe indexOf(">>if (a + b == 1) { }<< ")

      // Last part is the space
      root.parts.last shouldBe Space("if (a + b == 1) { }>> <<")
    }

    "parse `if` else `else` within other expressions" when {
      "the other expression is" when {
        "`while` statement" in {
          val root =
            parseSoft {
              """
                |while (if (a + b == 1) {true} else {false}) {
                |   if (true) {
                |     return break
                |   }
                |}
                |""".stripMargin
            }

          // the only non-space part is the `while` statement
          val parts = root.partsNonEmpty
          parts should have size 1
          val whileAST = parts.head.asInstanceOf[SoftAST.While]

          // check the `if-else` within the parentheses
          val ifElse = whileAST.expression.asInstanceOf[SoftAST.IfElse]
          ifElse.toCode() shouldBe "if (a + b == 1) {true} else {false}"

          // check the `if` within the block
          val blockParts = whileAST.block.value.partsNonEmpty
          blockParts should have size 1
          val blockIfAST = blockParts.head.asInstanceOf[SoftAST.IfElse]
          blockIfAST.toCode() shouldBe
            """if (true) {
              |     return break
              |   }""".stripMargin

        }
      }
    }
  }

}
