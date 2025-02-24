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
// along with the library. If not, see http:www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._

class InfixCallParserSpec extends AnyWordSpec with Matchers {

  "succeed" when {
    "left & right expressions are tuples" in {
      val infix =
        parseInfixCall("(one + one) <= (this - that)")

      val left = infix.leftExpression.asInstanceOf[SoftAST.Group[_, _]]
      left.toCode() shouldBe "(one + one)"

      val right = infix.rightExpression.asInstanceOf[SoftAST.Group[_, _]]
      right.toCode() shouldBe "(this - that)"

      infix.operator shouldBe LessThanOrEqual(indexOf("(one + one) >><=<< (this - that)"))
    }
  }

}
