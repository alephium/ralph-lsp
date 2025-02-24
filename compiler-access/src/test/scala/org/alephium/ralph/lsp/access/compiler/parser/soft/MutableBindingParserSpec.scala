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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class MutableBindingParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "mut is not followed by a space boundary" in {
      val mut =
        parseSoft("mutvariable")

      // it gets parsed as an identifier and not a mutable-binding
      mut.parts should have size 1
      mut.parts.head shouldBe
        Identifier(
          index = indexOf(">>mutvariable<<"),
          text = "mutvariable"
        )
    }
  }

  "succeed" when {
    "only mut is defined" in {
      val mut =
        parseMutableBinding("mut")

      mut shouldBe
        SoftAST.MutableBinding(
          index = indexOf(">>mut<<"),
          mut = Mut(indexOf(">>mut<<")),
          space = None,
          identifier = SoftAST.IdentifierExpected(indexOf("mut>><<"))
        )
    }

    "an identifier is set a mut" in {
      val mut =
        parseMutableBinding("mut variable")

      mut shouldBe
        SoftAST.MutableBinding(
          index = indexOf(">>mut variable<<"),
          mut = Mut(indexOf(">>mut<< variable")),
          space = Some(SpaceOne(indexOf("mut>> <<variable"))),
          identifier = Identifier(indexOf("mut >>variable<<"), "variable")
        )
    }

    "an identifier in a tuple is set a mut" in {
      val root =
        parseSoft("(a, b, mut variable)")

      root.parts should have size 1
      val tuple = root.parts.head.asInstanceOf[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]]

      tuple.headExpression shouldBe defined
      tuple.tailExpressions should have size 2 // there are two tail expressions
      val lastExpression = tuple.tailExpressions.last.expression.asInstanceOf[SoftAST.MutableBinding] // test the last expression i.e. `mut variable`

      lastExpression shouldBe
        SoftAST.MutableBinding(
          index = indexOf("(a, b, >>mut variable<<)"),
          mut = Mut(indexOf("(a, b, >>mut<< variable)")),
          space = Some(SpaceOne(indexOf("(a, b, mut>> <<variable)"))),
          identifier = Identifier(indexOf("(a, b, mut >>variable<<)"), "variable")
        )
    }

    "the binding is documented" when {
      "tuple" in {
        val root =
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

        root.parts should have size 2
        val tuple = root.parts.head.asInstanceOf[SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type]]

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
