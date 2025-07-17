// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
            leftExpression = Identifier(">>variable<< =="),
            preOperatorSpace = Some(Space("variable>> <<==")),
            operator = EqualEqual("variable >>==<<"),
            postOperatorSpace = None,
            rightExpression = ExpressionExpected("variable ==>><<")
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
            expressionLeft = Identifier(">>variable<<="),
            postIdentifierSpace = Some(Space("variable>> <<=")),
            equalToken = Equal("variable >>=<<"),
            postEqualSpace = None,
            expressionRight = ExpressionExpected("variable =>><<")
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
            expressionLeft = Identifier(">>variable<< = 1"),
            postIdentifierSpace = Some(Space("variable>> <<= 1")),
            equalToken = Equal("variable >>=<< 1"),
            postEqualSpace = Some(Space("variable =>> <<1")),
            expressionRight = Number(indexOf("variable = >>1<<"), "1")
          )
      }

      "expression is another expression" in {
        val assigment =
          parseAssignment("variable = variable + 1")

        assigment shouldBe
          SoftAST.Assignment(
            index = indexOf(">>variable = variable + 1<<"),
            expressionLeft = Identifier(">>variable<< = variable + 1"),
            postIdentifierSpace = Some(Space("variable>> <<= variable + 1")),
            equalToken = Equal("variable >>=<< variable + 1"),
            postEqualSpace = Some(Space("variable =>> << variable + 1")),
            expressionRight = SoftAST.InfixExpression(
              index = indexOf("variable = >>variable + 1<<"),
              leftExpression = Identifier("variable = >>variable<< + 1"),
              preOperatorSpace = Some(Space("variable = variable>> <<+ 1")),
              operator = Plus("variable = variable >>+<< 1"),
              postOperatorSpace = Some(Space("variable = variable +>> <<1")),
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
        val leftMethodCall = methodCall.leftExpression.asInstanceOf[SoftAST.MethodCall]
        leftMethodCall.toCode() shouldBe "obj.func(param)"
        val rightIdent = methodCall.rightExpression.asInstanceOf[SoftAST.Identifier]
        rightIdent.code.text shouldBe "counter"

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

        // right expression is a number
        assignment.expressionRight shouldBe
          ExpressionExpected("obj.func(param).counter =>><<")
      }
    }
  }

  "assignments to a tuple" should {
    "succeed" when {
      "right expression is a method call" in {
        val assignment = parseAssignment("(a, mut b, c) = cache.get(id).toTuple")

        val left = assignment.expressionLeft.asInstanceOf[SoftAST.Group[_, _, _]]
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
        val root = parseSoft("mut number = #00112233")

        root.parts should have size 1
        val assignment = root.parts.head.asInstanceOf[SoftAST.Assignment]

        val left = assignment.expressionLeft.asInstanceOf[SoftAST.MutableBinding]
        left.toCode() shouldBe "mut number"

        assignment.expressionRight shouldBe
          SoftAST.ByteVec(
            index = indexOf("mut number = >>#00112233<<"),
            hash = Hash("mut number = >>#<<00112233"),
            hex = Some(CodeString("mut number = #>>00112233<<"))
          )
      }

      "right expression is invalid" in {
        val root = parseSoft("""mut number = 😵""")

        root.parts should have size 2
        val assignment = root.parts.head.asInstanceOf[SoftAST.Assignment]
        val unresolved = root.parts.last.asInstanceOf[SoftAST.Unresolved]

        val left = assignment.expressionLeft.asInstanceOf[SoftAST.MutableBinding]
        left.toCode() shouldBe "mut number"

        assignment.expressionRight shouldBe ExpressionExpected("mut number = >><<😵")

        unresolved shouldBe
          Unresolved(
            index = indexOf("mut number = >>😵<<"),
            text = "😵"
          )
      }
    }
  }

}
