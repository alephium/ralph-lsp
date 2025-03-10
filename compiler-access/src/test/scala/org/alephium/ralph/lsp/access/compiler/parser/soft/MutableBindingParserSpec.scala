// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
          mut = Mut(">>mut<<"),
          space = None,
          identifier = IdentifierExpected("mut>><<")
        )
    }

    "an identifier is set a mut" in {
      val mut =
        parseMutableBinding("mut variable")

      mut shouldBe
        SoftAST.MutableBinding(
          index = indexOf(">>mut variable<<"),
          mut = Mut(">>mut<< variable"),
          space = Some(Space("mut>> <<variable")),
          identifier = Identifier("mut >>variable<<")
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
          mut = Mut("(a, b, >>mut<< variable)"),
          space = Some(Space("(a, b, mut>> <<variable)")),
          identifier = Identifier("(a, b, mut >>variable<<)")
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
