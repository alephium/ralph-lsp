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

class UnaryParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "not a unary operator" in {
      Token.reserved foreach {
        case _: Token.Unary =>
        // ignore unary operator

        case others =>
          // non-unary operators should fail
          assertIsFastParseError {
            parseUnary(others.lexeme)
          }
      }
    }

    "negative and positive numbers" should {
      "not get parsed as `Unary` because they are numbers" when {
        "negative number" in {
          val root = parseSoft("-1")
          root.parts should have size 1
          root.parts.head shouldBe a[SoftAST.Number]
        }

        "positive number" in {
          val root = parseSoft("+1")
          root.parts should have size 1
          root.parts.head shouldBe a[SoftAST.Number]
        }
      }
    }

  }

  "single empty unary operator" in {
    val ast = parseUnary("!")

    ast shouldBe
      SoftAST.Unary(
        index = indexOf(">>!<<"),
        unaryOperator = Exclamation(">>!<<"),
        preExpressionSpace = None,
        expression = ExpressionExpected("!>><<")
      )
  }

  "double empty unary operator" in {
    val ast = parseUnary("!!")

    ast shouldBe
      SoftAST.Unary(
        index = indexOf(">>!!<<"),
        unaryOperator = Exclamation(">>!<<!"),
        preExpressionSpace = None,
        expression = SoftAST.Unary(
          index = indexOf("!>>!<<"),
          unaryOperator = Exclamation("!>>!<<"),
          preExpressionSpace = None,
          expression = ExpressionExpected("!!>><<")
        )
      )
  }

  "function call" in {
    val ast = parseUnary("-account.getBalance")

    ast shouldBe
      SoftAST.Unary(
        index = indexOf(">>-account.getBalance<<"),
        unaryOperator = Minus(">>-<<account.getBalance"),
        preExpressionSpace = None,
        expression = SoftAST.MethodCall(
          index = indexOf("->>account.getBalance<<"),
          leftExpression = Identifier("->>account<<.getBalance"),
          preDotSpace = None,
          dot = Dot("-account>>.<<getBalance"),
          postDotSpace = None,
          rightExpression = Identifier("-account.>>getBalance<<"),
          preAssetApprovalSpace = None,
          assetApproval = None
        )
      )
  }

  "tupled boolean" when {
    "one element" in {
      val ast = parseUnary("!(true)")

      ast shouldBe
        SoftAST.Unary(
          index = indexOf(">>!(true)<<"),
          unaryOperator = Exclamation(">>!<<(true)"),
          preExpressionSpace = None,
          expression = SoftAST.Group(
            index = indexOf("!>>(true)<<"),
            openToken = Some(OpenParen("!>>(<<true)")),
            preHeadExpressionSpace = None,
            headExpression = Some(SoftAST.TokenExpression(True("!(>>true<<)"))),
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(CloseParen("!(true>>)<<"))
          )
        )
    }

    "nested unary" in {
      val ast = parseUnary("!(!true)")

      ast shouldBe
        SoftAST.Unary(
          index = indexOf(">>!(!true)<<"),
          unaryOperator = Exclamation(">>!<<(!true)"),
          preExpressionSpace = None,
          expression = SoftAST.Group(
            index = indexOf("!>>(!true)<<"),
            openToken = Some(OpenParen("!>>(<<!true)")),
            preHeadExpressionSpace = None,
            headExpression = Some(
              SoftAST.Unary(
                index = indexOf("!(>>!true<<)"),
                unaryOperator = Exclamation("!(>>!<<true)"),
                preExpressionSpace = None,
                expression = SoftAST.TokenExpression(True("!(!>>true<<)"))
              )
            ),
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(CloseParen("!(!true>>)<<"))
          )
        )
    }
  }

}
