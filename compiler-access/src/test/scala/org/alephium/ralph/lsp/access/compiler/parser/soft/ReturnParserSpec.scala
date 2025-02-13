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

class ReturnParserSpec extends AnyWordSpec with Matchers {

  "only the `return` keyword is specified" in {
    val returned =
      parseReturn("return")

    returned shouldBe
      SoftAST.Return(
        index = indexOf(">>return<<"),
        returnToken = Return(indexOf(">>return<<")),
        preExpressionSpace = None,
        rightExpression = SoftAST.ExpressionExpected(indexOf("return>><<"))
      )
  }

  "expression is an identifier" in {
    val returned =
      parseReturn("return value")

    returned shouldBe
      SoftAST.Return(
        index = indexOf(">>return value<<"),
        returnToken = Return(indexOf(">>return<< value")),
        preExpressionSpace = Some(SpaceOne(indexOf("return>> <<value"))),
        rightExpression = Identifier(indexOf("return >>value<<"), "value")
      )

  }

}
