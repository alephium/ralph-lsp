// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class ArrayAccessParserSpec extends AnyWordSpec with Matchers {

  "access index is missing" in {
    val ast =
      parseArrayAccess("array[]")

    ast shouldBe
      SoftAST.ArrayAccess(
        index = indexOf(">>array[]<<"),
        identifier = Identifier(">>array<<[]"),
        preOpenBracketSpace = None,
        openBracket = OpenBracket("array>>[<<]"),
        preAccessIndex = None,
        accessIndex = ExpressionExpected("array[>><<]"),
        preCloseBracketSpace = None,
        unresolved = None,
        closeBracket = BlockBracket("array[>>]<<")
      )
  }

  "access index is provided" in {
    val ast =
      parseArrayAccess("array[0]")

    ast shouldBe
      SoftAST.ArrayAccess(
        index = indexOf(">>array[0]<<"),
        identifier = Identifier(">>array<<[0]"),
        preOpenBracketSpace = None,
        openBracket = OpenBracket("array>>[<<0]"),
        preAccessIndex = None,
        accessIndex = Number("array[>>0<<]"),
        preCloseBracketSpace = None,
        unresolved = None,
        closeBracket = BlockBracket("array[0>>]<<")
      )
  }

  "newline exists before the index-access" in {
    val ast =
      parseArrayAccess {
        """
          |array
          |[0]
          |""".stripMargin.trim
      }

    ast shouldBe
      SoftAST.ArrayAccess(
        index = indexOf(
          """
            |>>array
            |[0]<<
            |""".stripMargin.trim
        ),
        identifier = Identifier(
          """
            |>>array<<
            |[0]
            |""".stripMargin.trim
        ),
        preOpenBracketSpace = Some(
          Space(
            """
              |array>>
              |<<[0]
              |""".stripMargin.trim
          )
        ),
        openBracket = OpenBracket(
          """
            |array
            |>>[<<0]
            |""".stripMargin.trim
        ),
        preAccessIndex = None,
        accessIndex = Number(
          """
            |array
            |[>>0<<]
            |""".stripMargin.trim
        ),
        preCloseBracketSpace = None,
        unresolved = None,
        closeBracket = BlockBracket(
          """
            |array
            |[0>>]<<
            |""".stripMargin.trim
        )
      )
  }

  "complex index-access via root SoftParser" in {
    val root =
      parseSoft("contract.function().array[map.getIndex(forValue)]")

    root.parts should have size 1
    val methodCall = root.parts.head.asInstanceOf[SoftAST.MethodCall]
    // Left expression is another method-call. Left does not require any further testing.
    methodCall.leftExpression.asInstanceOf[SoftAST.MethodCall].toCode() shouldBe "contract.function()"
    // Right expression is array access.
    val right = methodCall.rightExpression.asInstanceOf[SoftAST.ArrayAccess]
    right.toCode() shouldBe "array[map.getIndex(forValue)]"
    right.identifier shouldBe Identifier("contract.function().>>array<<[map.getIndex(forValue)]")
    // Right's accessed-index is also a method-call.
    val accessIndex = right.accessIndex.asInstanceOf[SoftAST.MethodCall]
    accessIndex.index shouldBe indexOf("contract.function().array[>>map.getIndex(forValue)<<]")
    accessIndex.toCode() shouldBe "map.getIndex(forValue)"
  }

  "infix call access" in {
    val ast =
      parseArrayAccess("array[1 + 2 + CONSTANT]")

    ast.accessIndex shouldBe a[SoftAST.InfixExpression]
    ast.accessIndex.toCode() shouldBe "1 + 2 + CONSTANT"
  }

  "method call access" in {
    val ast =
      parseArrayAccess("array[contract.function()]")

    ast.accessIndex shouldBe a[SoftAST.MethodCall]
    ast.accessIndex.toCode() shouldBe "contract.function()"
  }

  "unresolved expression" when {
    "annotation" in {
      val ast =
        parseArrayAccess("array[@stuff]")

      ast.unresolved.value.toCode() shouldBe "@stuff"
    }

    "space" in {
      val ast =
        parseArrayAccess("array[index blah]")

      ast.unresolved.value.toCode() shouldBe "blah"
    }

    "emoji" in {
      val ast =
        parseArrayAccess("array[🤙]")

      ast.unresolved.value.toCode() shouldBe "🤙"
    }

    "valid code followed by emoji" when {
      "identifier" in {
        val ast =
          parseArrayAccess("array[index 🤙]")

        ast.unresolved.value.toCode() shouldBe "🤙"
      }

      "infix expression" in {
        val ast =
          parseArrayAccess("array[1 + 2 🤙]")

        ast.unresolved.value.toCode() shouldBe "🤙"
      }
    }

    "closing bracket is missing" in {
      val ast = parseArrayAccess("array[🤙")

      ast.unresolved.value.toCode() shouldBe "🤙"
      ast.closeBracket shouldBe TokenExpected("array[🤙>><<", Token.BlockBracket)
    }
  }

}
