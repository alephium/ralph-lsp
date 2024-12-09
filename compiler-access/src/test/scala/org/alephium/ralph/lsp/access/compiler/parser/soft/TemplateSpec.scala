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
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class TemplateSpec extends AnyWordSpec with Matchers {

  "parse template type" when {
    def testTokenIsReportedUnresolved(templateToken: String) = {
      val block = parseSoft(templateToken)

      val expected =
        SoftAST.BlockBody(
          index = indexOf(s">>$templateToken<<"),
          prePartsSpace = None,
          parts = Seq(
            SoftAST.BlockBodyPart(
              index = indexOf(s">>$templateToken<<"),
              part = SoftAST.Unresolved(templateToken, indexOf(s">>$templateToken<<")),
              postPartSpace = None
            )
          )
        )

      block shouldBe expected
    }

    "Contract" when {
      "camelcase" in {
        val templateToken =
          "Contract"

        val template =
          parseTemplate(templateToken)

        template.index shouldBe indexOf(s">>$templateToken<<")
        template.templateType shouldBe SoftAST.Contract(indexOf(s">>$templateToken<<"))
        template.preIdentifierSpace shouldBe SoftAST.SpaceExpected(indexOf(s"$templateToken>><<"))
        template.identifier shouldBe SoftAST.IdentifierExpected(indexOf(s"$templateToken>><<"))
        template.preParamSpace shouldBe None
        template.params shouldBe a[SoftAST.NonEmptyParameterClause] // TODO: This should default to Empty when parens are missing
        template.postParamSpace shouldBe None
        template.block.openCurly shouldBe SoftAST.OpenCurlyExpected(indexOf(s"$templateToken>><<"))
        template.block.closeCurly shouldBe SoftAST.CloseCurlyExpected(indexOf(s"$templateToken>><<"))
      }

      "lowercase" in {
        testTokenIsReportedUnresolved("contract")
      }
    }

    "TxScript" when {
      "camelcase" in {
        val templateToken =
          "TxScript"

        val template =
          parseTemplate(templateToken)

        template.index shouldBe indexOf(s">>$templateToken<<")
        template.templateType shouldBe SoftAST.TxScript(indexOf(s">>$templateToken<<"))
        template.preIdentifierSpace shouldBe SoftAST.SpaceExpected(indexOf(s"$templateToken>><<"))
        template.identifier shouldBe SoftAST.IdentifierExpected(indexOf(s"$templateToken>><<"))
        template.preParamSpace shouldBe None
        template.params shouldBe a[SoftAST.NonEmptyParameterClause] // TODO: This should default to Empty when parens are missing
        template.postParamSpace shouldBe None
        template.block.openCurly shouldBe SoftAST.OpenCurlyExpected(indexOf(s"$templateToken>><<"))
        template.block.closeCurly shouldBe SoftAST.CloseCurlyExpected(indexOf(s"$templateToken>><<"))
      }

      "lowercase" in {
        testTokenIsReportedUnresolved("txscript")
      }
    }
  }

  "parse template identifier" in {
    val template =
      parseTemplate("Contract mycontract")

    template.identifier shouldBe SoftAST.Identifier("mycontract", indexOf("Contract >>mycontract<<"))
  }

  "parse params" when {
    "open closing paren is missing" in {
      val template =
        parseTemplate("Contract mycontract(")

      val params = template.params.asInstanceOf[SoftAST.NonEmptyParameterClause]
      params.openParen shouldBe SoftAST.OpenParen(indexOf("Contract mycontract>>(<<"))
      params.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("Contract mycontract(>><<"))
    }

    "params are empty" in {
      val template =
        parseTemplate("Contract mycontract( )")

      val params = template.params.asInstanceOf[SoftAST.EmptyParameterClause]
      params.preCloseParenSpace.value shouldBe SoftAST.Space(" ", indexOf("Contract mycontract(>> <<)"))
    }
  }

  "parse block" when {
    "open brace is missing" in {
      val template =
        parseTemplate("Contract mycontract }")

      template.block.closeCurly shouldBe SoftAST.CloseCurly(indexOf("Contract mycontract >>}<<"))
      template.block.openCurly shouldBe SoftAST.OpenCurlyExpected(indexOf("Contract mycontract >><<}"))
    }

    "close brace and identifier are missing" in {
      val template =
        parseTemplate("Contract {")

      template.identifier shouldBe SoftAST.IdentifierExpected(indexOf("Contract >><<{"))
      template.preIdentifierSpace shouldBe SoftAST.Space(" ", indexOf("Contract>> <<{"))
      template.block.openCurly shouldBe SoftAST.OpenCurly(indexOf("Contract >>{<<"))
      template.block.closeCurly shouldBe SoftAST.CloseCurlyExpected(indexOf("Contract {>><<"))
    }

    "a function is defined" in {
      val template =
        parseTemplate {
          """Contract {
            |  fn function( ->
            |
            |""".stripMargin
        }

      template.identifier shouldBe SoftAST.IdentifierExpected(indexOf("Contract >><<{"))
      template.preIdentifierSpace shouldBe SoftAST.Space(" ", indexOf("Contract>> <<{"))
      // block
      template.block.openCurly shouldBe SoftAST.OpenCurly(indexOf("Contract >>{<<"))
      template.block.closeCurly shouldBe
        SoftAST.CloseCurlyExpected(
          indexOf("""Contract {
              |  fn function( ->
              |
              |>><<""".stripMargin)
        )

      /**
       * Body part: Function
       */
      template.block.body.parts should have size 1
      val function = template.block.body.parts.head.part.asInstanceOf[SoftAST.Function]

      function.fn shouldBe
        SoftAST.Fn(
          indexOf("""Contract {
              |  >>fn<< function( ->
              |
              |""".stripMargin)
        )

      function.signature.fnName shouldBe
        SoftAST.Identifier(
          "function",
          indexOf("""Contract {
                    |  fn >>function<<( ->
                    |
                    |""".stripMargin)
        )
    }

    "a TxScript is defined within a Contract" in {
      val template =
        parseTemplate {
          """Contract {
            |  TxScript myScript
            |
            |""".stripMargin
        }

      template.identifier shouldBe SoftAST.IdentifierExpected(indexOf("Contract >><<{"))
      template.preIdentifierSpace shouldBe SoftAST.Space(" ", indexOf("Contract>> <<{"))
      // block
      template.block.openCurly shouldBe SoftAST.OpenCurly(indexOf("Contract >>{<<"))
      template.block.closeCurly shouldBe
        SoftAST.CloseCurlyExpected(
          indexOf("""Contract {
                    |  TxScript myScript
                    |
                    |>><<""".stripMargin)
        )

      /**
       * Body part: TxScript
       */
      template.block.body.parts should have size 1
      val txScriptTemplate = template.block.body.parts.head.part.asInstanceOf[SoftAST.Template]

      txScriptTemplate.templateType shouldBe
        SoftAST.TxScript(
          indexOf("""Contract {
                    |  >>TxScript<< myScript
                    |
                    |""".stripMargin)
        )

      txScriptTemplate.identifier shouldBe
        SoftAST.Identifier(
          "myScript",
          indexOf("""Contract {
                    |  TxScript >>myScript<<
                    |
                    |""".stripMargin)
        )
    }

    "an unresolved token is defined within a TxScript" in {
      val template =
        parseTemplate {
          """Contract MyContract {
            |  blah
            |
            |""".stripMargin
        }

      /**
       * Body part: TxScript
       */
      template.block.body.parts should have size 1
      val part = template.block.body.parts.head.part

      part shouldBe
        SoftAST.Unresolved(
          "blah",
          indexOf("""Contract MyContract {
                    |  >>blah<<
                    |
                    |""".stripMargin)
        )

    }
  }

}