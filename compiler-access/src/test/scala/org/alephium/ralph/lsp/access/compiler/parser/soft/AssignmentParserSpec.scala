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
import org.alephium.ralph.lsp.access.util.TestFastParse.assertIsFastParseError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AssignmentParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "equality token `==` is used instead of `=`" when {
      "parsed from root" in {
        val root =
          parseSoft("variable ==")

        root.parts should have size 1
        val infix = root.parts.head

        // `==` is not an assignment so it should get parsed as an infix expression
        infix shouldBe
          SoftAST.InfixExpression(
            index = indexOf(">>variable ==<<"),
            leftExpression = Identifier(indexOf(">>variable<< =="), "variable"),
            preOperatorSpace = Some(SpaceOne(indexOf("variable>> <<=="))),
            operator = EqualEqual(indexOf("variable >>==<<")),
            postOperatorSpace = None,
            rightExpression = SoftAST.ExpressionExpected(indexOf("variable ==>><<"))
          )
      }

      "parsed by AssignmentParser" in {
        // executing it directly on `AssignmentParser` should result in failed parser
        assertIsFastParseError {
          parseAssignment("variable ==")
        }
      }
    }
  }

  "assignments to an identifier" should {
    "report ExpressionExpected" when {
      "a variable is assigned without initialisation" in {
        val assignment =
          parseAssignment("variable =")

        assignment shouldBe
          SoftAST.Assignment(
            index = indexOf(">>variable =<<"),
            expressionLeft = Identifier(indexOf(">>variable<<="), "variable"),
            postIdentifierSpace = Some(SpaceOne(indexOf("variable>> <<="))),
            equalToken = Equal(indexOf("variable >>=<<")),
            postEqualSpace = None,
            expressionRight = SoftAST.ExpressionExpected(indexOf("variable =>><<"))
          )
      }
    }

    "succeed" when {
      "full assignment syntax is defined" in {
        val assigment =
          parseAssignment("variable = 1")

        assigment shouldBe
          SoftAST.Assignment(
            index = indexOf(">>variable = 1<<"),
            expressionLeft = Identifier(indexOf(">>variable<< = 1"), "variable"),
            postIdentifierSpace = Some(SpaceOne(indexOf("variable>> <<= 1"))),
            equalToken = Equal(indexOf("variable >>=<< 1")),
            postEqualSpace = Some(SpaceOne(indexOf("variable =>> <<1"))),
            expressionRight = Number(indexOf("variable = >>1<<"), "1")
          )
      }

      "expression is another expression" in {
        val assigment =
          parseAssignment("variable = variable + 1")

        assigment shouldBe
          SoftAST.Assignment(
            index = indexOf(">>variable = variable + 1<<"),
            expressionLeft = Identifier(indexOf(">>variable<< = variable + 1"), "variable"),
            postIdentifierSpace = Some(SpaceOne(indexOf("variable>> <<= variable + 1"))),
            equalToken = Equal(indexOf("variable >>=<< variable + 1")),
            postEqualSpace = Some(SpaceOne(indexOf("variable =>> << variable + 1"))),
            expressionRight = SoftAST.InfixExpression(
              index = indexOf("variable = >>variable + 1<<"),
              leftExpression = Identifier(indexOf("variable = >>variable<< + 1"), "variable"),
              preOperatorSpace = Some(SpaceOne(indexOf("variable = variable>> <<+ 1"))),
              operator = Plus(indexOf("variable = variable >>+<< 1")),
              postOperatorSpace = Some(SpaceOne(indexOf("variable = variable +>> <<1"))),
              rightExpression = Number(indexOf("variable = variable + >>1<<"), "1")
            )
          )
      }
    }
  }

  "assignments to an expression" should {
    "succeed" when {
      "left expression is a method call" in {
        val assignment =
          parseAssignment("obj.func(param).counter = 0")

        // left expression is a method call
        val methodCall = assignment.expressionLeft.asInstanceOf[SoftAST.MethodCall]
        methodCall.index shouldBe indexOf(">>obj.func(param).counter<< = 0")
        val objectName = methodCall.leftExpression.asInstanceOf[SoftAST.Identifier]
        objectName.code.text shouldBe "obj"

        // right expression is a number
        val number = assignment.expressionRight.asInstanceOf[SoftAST.Number]
        number shouldBe
          Number(
            index = indexOf("obj.func(param).counter = >>0<<"),
            text = "0"
          )
      }

      "left & right expressions both are method call" in {
        val assignment =
          parseAssignment("obj.func(param).counter = cache.getNumber()")

        // left expression is a method call
        val left = assignment.expressionLeft.asInstanceOf[SoftAST.MethodCall]
        left.index shouldBe indexOf(">>obj.func(param).counter<< = cache.getNumber()")
        val objectName = left.leftExpression.asInstanceOf[SoftAST.Identifier]
        objectName.code.text shouldBe "obj"

        // right expression is also a method call
        val right = assignment.expressionRight.asInstanceOf[SoftAST.MethodCall]
        right.index shouldBe indexOf("obj.func(param).counter = >>cache.getNumber()<<")
        val cacheObject = right.leftExpression.asInstanceOf[SoftAST.Identifier]
        cacheObject.code.text shouldBe "cache"
      }
    }

    "report missing expression" when {
      "left expression is a method call and right expression is missing" in {
        val assignment =
          parseAssignment("obj.func(param).counter =")

        // left expression is a method call
        val methodCall = assignment.expressionLeft.asInstanceOf[SoftAST.MethodCall]
        methodCall.index shouldBe indexOf(">>obj.func(param).counter<< = 0")
        val objectName = methodCall.leftExpression.asInstanceOf[SoftAST.Identifier]
        objectName.code.text shouldBe "obj"

        // right expression is a number
        assignment.expressionRight shouldBe
          SoftAST.ExpressionExpected(indexOf("obj.func(param).counter =>><<"))
      }
    }
  }

  "assignments to a tuple" should {
    "succeed" when {
      "right expression is a method call" in {
        val assignment = parseAssignment("(a, mut b, c) = cache.get(id).toTuple")

        val left = assignment.expressionLeft.asInstanceOf[SoftAST.Group[_, _]]
        left.toCode() shouldBe "(a, mut b, c)"

        val right = assignment.expressionRight.asInstanceOf[SoftAST.MethodCall]
        right.toCode() shouldBe "cache.get(id).toTuple"
      }
    }
  }

  "assignments to a mutable binding" should {
    "succeed" when {
      "right expression is a variable" in {
        val assignment = parseAssignment("mut number = -1e18")

        val left = assignment.expressionLeft.asInstanceOf[SoftAST.MutableBinding]
        left.toCode() shouldBe "mut number"

        val right = assignment.expressionRight.asInstanceOf[SoftAST.Number]
        right.toCode() shouldBe "-1e18"
      }

      "right expression is a ByteVec" in {
        // ByteVec is valid syntax, but has no parser implemented.
        // Until then, ByteVec is reported as Unresolved.
        val root = parseSoft("mut number = #00112233")

        root.parts should have size 2
        val assignment = root.parts.head.asInstanceOf[SoftAST.Assignment]
        val unresolved = root.parts.last.asInstanceOf[SoftAST.Unresolved]

        val left = assignment.expressionLeft.asInstanceOf[SoftAST.MutableBinding]
        left.toCode() shouldBe "mut number"

        assignment.expressionRight shouldBe SoftAST.ExpressionExpected(indexOf("mut number = >><<#00112233"))

        unresolved shouldBe
          Unresolved(
            index = indexOf("mut number = >>#00112233<<"),
            text = "#00112233"
          )
      }
    }
  }

}
