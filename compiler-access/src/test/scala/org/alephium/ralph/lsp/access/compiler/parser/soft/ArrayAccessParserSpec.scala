// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

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
        closeBracket = BlockBracket(
          """
            |array
            |[0>>]<<
            |""".stripMargin.trim
        )
      )
  }

}
