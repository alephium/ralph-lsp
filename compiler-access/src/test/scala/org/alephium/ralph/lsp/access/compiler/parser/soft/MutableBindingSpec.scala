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

class MutableBindingSpec extends AnyWordSpec with Matchers {

  "succeed" when {
    "an identifier is set a mut" in {
      val annotation =
        parseMutableBinding("mut variable")

      annotation shouldBe
        SoftAST.MutableBinding(
          index = indexOf(">>mut variable<<"),
          mut = Mut(indexOf(">>mut<< variable")),
          space = SpaceOne(indexOf("mut>> <<variable")),
          identifier = Identifier(indexOf("mut >>variable<<"), "variable")
        )
    }

    "an identifier in a tuple is set a mut" in {
      val body =
        parseSoft("(a, b, mut variable)")

      body.parts should have size 1
      val tuple = body.parts.head.part.asInstanceOf[SoftAST.Tuple]

      tuple.headExpression shouldBe defined
      tuple.tailExpressions should have size 2 // there are two tail expressions
      val lastExpression = tuple.tailExpressions.last.expression.asInstanceOf[SoftAST.MutableBinding] // test the last expression i.e. `mut variable`

      lastExpression shouldBe
        SoftAST.MutableBinding(
          index = indexOf("(a, b, >>mut variable<<)"),
          mut = Mut(indexOf("(a, b, >>mut<< variable)")),
          space = SpaceOne(indexOf("(a, b, mut>> <<variable)")),
          identifier = Identifier(indexOf("(a, b, mut >>variable<<)"), "variable")
        )
    }

    "the binding is documented" when {
      "tuple" in {
        val body =
          parseSoft {
            """(  
              |a,
              |b,
              |// documentation line 1
              |// documentation line 2
              |mut variable
              |)
              |""".stripMargin
          }

        body.parts should have size 1
        val tuple = body.parts.head.part.asInstanceOf[SoftAST.Tuple]

        tuple.headExpression shouldBe defined
        tuple.tailExpressions should have size 2 // there are two tail expressions
        val lastExpression = tuple.tailExpressions.last.expression.asInstanceOf[SoftAST.MutableBinding] // test the last expression i.e. `mut variable`

        lastExpression.mut.documentation.value.toCode() shouldBe
          """// documentation line 1
            |// documentation line 2
            |""".stripMargin
      }
    }
  }

}
