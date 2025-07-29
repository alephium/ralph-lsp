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

}
