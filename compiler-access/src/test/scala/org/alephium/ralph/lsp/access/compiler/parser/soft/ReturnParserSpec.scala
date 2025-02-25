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
            postHeadExpressionSpace = None,
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
            postHeadExpressionSpace = None,
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
