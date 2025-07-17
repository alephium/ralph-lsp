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

class ArrowAssignmentParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "arrow is not defined" in {
      assertIsFastParseError {
        parseArrowAssignment("user - 1 alph")
      }
    }
  }

  "left expression is missing" in {
    val ast = parseArrowAssignment("-> 1 alph")

    ast shouldBe
      SoftAST.ArrowAssignment(
        index = indexOf(">>-> 1 alph<<"),
        leftExpression = ExpressionExpected(">><<-> 1 alph"),
        preArrowSpace = None,
        forwardArrow = ForwardArrow(">>-><< 1 alph"),
        preRightExpressionSpace = Some(Space("  >> <<1 alph")),
        rightExpression = SoftAST.Number(
          index = indexOf("-> >>1 alph<<"),
          documentation = None,
          number = CodeString("-> >>1<< alph"),
          unit = Some(
            SoftAST.UnitAlph(
              index = indexOf("-> 1>> alph<<"),
              space = Some(Space("-> 1>> <<alph")),
              unit = TokenDocumented(indexOf("-> 1 >>alph<<"), Token.AlphLowercase)
            )
          )
        )
      )
  }

  "left expression is unresolved" in {
    val ast = parseArrowAssignment("$ -> 1 alph")

    ast shouldBe
      SoftAST.ArrowAssignment(
        index = indexOf(">>$ -> 1 alph<<"),
        leftExpression = Unresolved(">>$<< -> 1 alph"),
        preArrowSpace = Some(Space("$>> <<-> 1 alph")),
        forwardArrow = ForwardArrow("$ >>-><< 1 alph"),
        preRightExpressionSpace = Some(Space("$ - >> <<1 alph")),
        rightExpression = SoftAST.Number(
          index = indexOf("$ -> >>1 alph<<"),
          documentation = None,
          number = CodeString("$ -> >>1<< alph"),
          unit = Some(
            SoftAST.UnitAlph(
              index = indexOf("$ -> 1>> alph<<"),
              space = Some(Space("$ -> 1>> <<alph")),
              unit = TokenDocumented(indexOf("$ -> 1 >>alph<<"), Token.AlphLowercase)
            )
          )
        )
      )
  }

  "right expression is missing" in {
    val ast = parseArrowAssignment("$ ->")

    ast shouldBe
      SoftAST.ArrowAssignment(
        index = indexOf(">>$ -><<"),
        leftExpression = Unresolved(">>$<< ->"),
        preArrowSpace = Some(Space("$>> <<->")),
        forwardArrow = ForwardArrow("$ >>-><<"),
        preRightExpressionSpace = None,
        rightExpression = ExpressionExpected("$ - >><<")
      )
  }

}
