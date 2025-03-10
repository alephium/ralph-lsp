// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InfixCallParserSpec extends AnyWordSpec with Matchers {

  "succeed" when {
    "left & right expressions are tuples" in {
      val infix =
        parseInfixCall("(one + one) <= (this - that)")

      val left = infix.leftExpression.asInstanceOf[SoftAST.Group[_, _]]
      left.toCode() shouldBe "(one + one)"

      val right = infix.rightExpression.asInstanceOf[SoftAST.Group[_, _]]
      right.toCode() shouldBe "(this - that)"

      infix.operator shouldBe LessThanOrEqual("(one + one) >><=<< (this - that)")
    }
  }

}
