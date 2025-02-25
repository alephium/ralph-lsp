// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse.assertIsFastParseError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConstParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "const is not a keyword" in {
      assertIsFastParseError {
        parseConst("constant")
      }
    }
  }

  "pass" when {
    "only const is defined" in {
      val const = parseConst("const")

      const shouldBe
        SoftAST.Const(
          index = indexOf(">>const<<"),
          constToken = Const(">>const<<"),
          preAssignmentSpace = None,
          assignment = SoftAST.Assignment(
            index = indexOf("const>><<"),
            expressionLeft = ExpressionExpected("const>><<"),
            postIdentifierSpace = None,
            equalToken = TokenExpected("const>><<", Token.Equal),
            postEqualSpace = None,
            expressionRight = ExpressionExpected("const>><<")
          )
        )
    }

    "only const and identifier are defined" in {
      val const = parseConst("const value")

      const shouldBe
        SoftAST.Const(
          index = indexOf(">>const value<<"),
          constToken = Const(">>const<< value"),
          preAssignmentSpace = Some(Space("const>> <<value")),
          assignment = SoftAST.Assignment(
            index = indexOf("const >>value<<"),
            expressionLeft = Identifier("const >>value<<"),
            postIdentifierSpace = None,
            equalToken = TokenExpected("const value>><<", Token.Equal),
            postEqualSpace = None,
            expressionRight = ExpressionExpected("const value>><<")
          )
        )
    }

    "fully defined" when {
      "right expression is a number" in {
        val const = parseConst("const value = 1")

        const shouldBe
          SoftAST.Const(
            index = indexOf(">>const value = 1<<"),
            constToken = Const(">>const<< value = 1"),
            preAssignmentSpace = Some(Space("const>> <<value = 1")),
            assignment = SoftAST.Assignment(
              index = indexOf("const >>value = 1<<"),
              expressionLeft = Identifier("const >>value<< = 1"),
              postIdentifierSpace = Some(Space("const value>> <<= 1")),
              equalToken = Equal("const value >>=<< 1"),
              postEqualSpace = Some(Space("const value =>> <<1")),
              expressionRight = Number("const value = >>1<<")
            )
          )
      }

      "another expression is a number" in {
        val const = parseConst("const value = (1 + 2) * contract.call().value")

        // This test is for `const` only, so asserting the AST of the right-hand expression is not necessary.
        const.copy(assignment = null) shouldBe
          SoftAST.Const(
            index = indexOf(">>const value = (1 + 2) * contract.call().value<<"),
            constToken = Const(">>const<< value = (1 + 2) * contract.call().value"),
            preAssignmentSpace = Some(Space("const>> <<value = (1 + 2) * contract.call().value")),
            assignment = null
          )

        // Assert that the right-hand expression produces the expected code.
        const.assignment.toCode() shouldBe "value = (1 + 2) * contract.call().value"
      }
    }
  }

}
