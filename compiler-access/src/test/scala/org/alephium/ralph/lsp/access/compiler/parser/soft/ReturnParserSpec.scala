// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class ReturnParserSpec extends AnyWordSpec with Matchers {

  "only the `return` keyword is specified" in {
    val returned =
      parseReturn("return")

    returned shouldBe
      SoftAST.Return(
        index = indexOf(">>return<<"),
        returnToken = Return(">>return<<"),
        preExpressionSpace = None,
        rightExpression = ExpressionExpected("return>><<")
      )
  }

  "expression is an identifier" in {
    val returned =
      parseReturn("return value")

    returned shouldBe
      SoftAST.Return(
        index = indexOf(">>return value<<"),
        returnToken = Return(">>return<< value"),
        preExpressionSpace = Some(Space("return>> <<value")),
        rightExpression = Identifier("return >>value<<")
      )
  }

  "successfully parse a returned tuple" when {
    "tail element is not provided" in {
      val returned =
        parseReturn("return value,")

      returned shouldBe
        SoftAST.Return(
          index = indexOf(">>return value,<<"),
          returnToken = Return(">>return<< value,"),
          preExpressionSpace = Some(Space("return>> <<value,")),
          rightExpression = SoftAST.Group(
            index = indexOf("return >>value,<<"),
            openToken = None,
            preHeadExpressionSpace = None,
            headExpression = Some(Identifier("return >>value<<,")),
            preTailExpressionSpace = None,
            tailExpressions = Seq(
              SoftAST.GroupTail(
                index = indexOf("return value>>,<<"),
                comma = Comma("return value>>,<<"),
                preExpressionSpace = None,
                expression = ExpressionExpected("return value,>><<"),
                postExpressionSpace = None
              )
            ),
            closeToken = None
          )
        )
    }

    "head element is not provided" in {
      val returned =
        parseReturn("return ,value")

      returned shouldBe
        SoftAST.Return(
          index = indexOf(">>return ,value<<"),
          returnToken = Return(">>return<< ,value"),
          preExpressionSpace = Some(Space("return>> <<,value")),
          rightExpression = SoftAST.Group(
            index = indexOf("return >>,value<<"),
            openToken = None,
            preHeadExpressionSpace = None,
            headExpression = Some(ExpressionExpected("return >><<,value")),
            preTailExpressionSpace = None,
            tailExpressions = Seq(
              SoftAST.GroupTail(
                index = indexOf("return >>,value<<"),
                comma = Comma("return >>,<<value"),
                preExpressionSpace = None,
                expression = Identifier("return ,>>value<<"),
                postExpressionSpace = None
              )
            ),
            closeToken = None
          )
        )
    }

    "valid tuple is provided" in {
      val returned =
        parseReturn("return value, (1 + 2), function.get(1)")

      returned.returnToken shouldBe Return(">>return<< value, (1 + 2), function.get(1)")

      // Assert the Group's code
      val group = returned.rightExpression.asInstanceOf[SoftAST.Group[Nothing, Nothing]]
      group.toCode() shouldBe "value, (1 + 2), function.get(1)"

      // Assert the Group's head expression
      group.headExpression.value.toCode() shouldBe "value"

      // Assert the Group's tail expressions
      group.tailExpressions should have size 2
      // Tail's head expression
      group.tailExpressions.head.expression shouldBe a[SoftAST.Group[_, _]]
      group.tailExpressions.head.expression.toCode() shouldBe "(1 + 2)"
      // Tail's last expression
      group.tailExpressions.last.expression shouldBe a[SoftAST.MethodCall]
      group.tailExpressions.last.expression.toCode() shouldBe "function.get(1)"
    }
  }

}
