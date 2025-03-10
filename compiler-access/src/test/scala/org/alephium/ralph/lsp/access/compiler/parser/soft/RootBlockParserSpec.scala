// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RootBlockParserSpec extends AnyWordSpec with Matchers {

  "not wrap a single expression within an ExpressionBlock" when {
    "only one expression exists" in {
      val root =
        parseRootBlock {
          """
            |let one = 1
            |""".stripMargin
        }

      // the body contains only a single element
      root.parts should have size 3
      // that element is a `VariableDeclaration` and not an `ExpressionBlock`.
      val variableDeclaration = root.parts(1).asInstanceOf[SoftAST.VariableDeclaration]
      variableDeclaration.toCode() shouldBe "let one = 1"
    }
  }

  "two expressions" should {
    "wrap multiple expression within ExpressionBlock" in {
      val root =
        parseRootBlock {
          """
            |let one = 1
            |true == true
            |""".stripMargin
        }

      // now that the root contains two expressions, it should wrap it in an ExpressionBlock
      // so it still contains only one element
      root.parts should have size 3
      val expressionBlock = root.parts(1).asInstanceOf[SoftAST.ExpressionBlock]

      // head expression contains the variable declaration
      expressionBlock.headExpression shouldBe a[SoftAST.VariableDeclaration]
      expressionBlock.headExpression.toCode() shouldBe "let one = 1"

      // tail expression also has size one as it contains only the infix equal check
      expressionBlock.tailExpressions should have size 1
      expressionBlock.tailExpressions.head.expression shouldBe a[SoftAST.InfixExpression]
      expressionBlock.tailExpressions.head.expression.toCode() shouldBe "true == true"
    }
  }

}
