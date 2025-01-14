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

class AssignmentSpec extends AnyWordSpec with Matchers {

  "report ExpressionExpected" when {
    "a variable is assigned without initialisation" in {
      val annotation =
        parseAssignment("variable =")

      annotation shouldBe
        SoftAST.Assignment(
          index = indexOf(">>variable =<<"),
          identifier = Identifier(indexOf(">>variable<<="), "variable"),
          postIdentifierSpace = Some(SpaceOne(indexOf("variable>> <<="))),
          equalToken = Equal(indexOf("variable >>=<<")),
          postEqualSpace = None,
          expression = SoftAST.ExpressionExpected(indexOf("variable =>><<"))
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
          identifier = Identifier(indexOf(">>variable<< = 1"), "variable"),
          postIdentifierSpace = Some(SpaceOne(indexOf("variable>> <<= 1"))),
          equalToken = Equal(indexOf("variable >>=<< 1")),
          postEqualSpace = Some(SpaceOne(indexOf("variable =>> <<1"))),
          expression = Number(indexOf("variable = >>1<<"), "1")
        )
    }

    "expression is another expression" in {
      val assigment =
        parseAssignment("variable = variable + 1")

      assigment shouldBe
        SoftAST.Assignment(
          index = indexOf(">>variable = variable + 1<<"),
          identifier = Identifier(indexOf(">>variable<< = variable + 1"), "variable"),
          postIdentifierSpace = Some(SpaceOne(indexOf("variable>> <<= variable + 1"))),
          equalToken = Equal(indexOf("variable >>=<< variable + 1")),
          postEqualSpace = Some(SpaceOne(indexOf("variable =>> << variable + 1"))),
          expression = SoftAST.InfixExpression(
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
