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

class TemplateParserSpec extends AnyWordSpec with Matchers {

  "parse template type" when {
    def testTokenIsReportedAsIdentifier(templateToken: String) = {
      val block = parseSoft(templateToken)

      val expected =
        SoftAST.BlockBody(
          index = indexOf(s">>$templateToken<<"),
          prePartsSpace = None,
          parts = Seq(
            SoftAST.BlockBodyPart(
              index = indexOf(s">>$templateToken<<"),
              part = Identifier(
                index = indexOf(s">>$templateToken<<"),
                text = templateToken
              ),
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
        template.templateType shouldBe Contract(indexOf(s">>$templateToken<<"))
        template.preIdentifierSpace shouldBe None
        template.identifier shouldBe SoftAST.IdentifierExpected(indexOf(s"$templateToken>><<"))
        template.preParamSpace shouldBe None
        template.params shouldBe empty
        template.postParamSpace shouldBe None
        template.block.openCurly shouldBe SoftAST.TokenExpected(indexOf(s"$templateToken>><<"), Token.OpenCurly)
        template.block.closeCurly shouldBe SoftAST.TokenExpected(indexOf(s"$templateToken>><<"), Token.CloseCurly)
      }

      "lowercase" in {
        testTokenIsReportedAsIdentifier("contract")
      }
    }

    "TxScript" when {
      "camelcase" in {
        val templateToken =
          "TxScript"

        val template =
          parseTemplate(templateToken)

        template.index shouldBe indexOf(s">>$templateToken<<")
        template.templateType shouldBe TxScript(indexOf(s">>$templateToken<<"))
        template.preIdentifierSpace shouldBe None
        template.identifier shouldBe SoftAST.IdentifierExpected(indexOf(s"$templateToken>><<"))
        template.preParamSpace shouldBe empty
        template.params shouldBe empty
        template.postParamSpace shouldBe empty
        template.block.openCurly shouldBe SoftAST.TokenExpected(indexOf(s"$templateToken>><<"), Token.OpenCurly)
        template.block.closeCurly shouldBe SoftAST.TokenExpected(indexOf(s"$templateToken>><<"), Token.CloseCurly)
      }

      "lowercase" in {
        testTokenIsReportedAsIdentifier("txscript")
      }
    }
  }

  "parse template identifier" in {
    val template =
      parseTemplate("Contract mycontract")

    template.identifier shouldBe
      Identifier(
        index = indexOf("Contract >>mycontract<<"),
        text = "mycontract"
      )
  }

  "parse params" when {
    "open closing paren is missing" in {
      val template =
        parseTemplate("Contract mycontract(")

      val params = template.params.value
      params.openToken shouldBe OpenParen(indexOf("Contract mycontract>>(<<"))
      params.closeToken shouldBe SoftAST.TokenExpected(indexOf("Contract mycontract(>><<"), Token.CloseParen)
    }

    "params are empty" in {
      val template =
        parseTemplate("Contract mycontract( )")

      val params = template.params.value

      params.preHeadExpressionSpace.value shouldBe SpaceOne(indexOf("Contract mycontract(>> <<)"))
    }
  }

  "parse inheritance" when {
    "implements without defining implementing class" in {
      val template =
        parseTemplate("Contract MyContract implements")

      template.inheritance should contain only
        SoftAST.Inheritance(
          index = indexOf("Contract MyContract >>implements<<"),
          inheritanceType = Implements(indexOf("Contract MyContract >>implements<<")),
          postInheritanceTypeSpace = None,
          headReference = SoftAST.IdentifierExpected(indexOf("Contract MyContract implements>><<")),
          tailReferencesOrSpace = None
        )
    }

    "implements & extends have single reference" in {
      val template =
        parseTemplate("Contract HelloWorld extends Class implements Trait")

      template.inheritance should have size 2

      val extended   = template.inheritance.head
      val implements = template.inheritance.last

      /** Assert extends */
      extended shouldBe
        SoftAST.Inheritance(
          index = indexOf("Contract HelloWorld >>extends Class <<implements Trait"),
          inheritanceType = Extends(indexOf("Contract HelloWorld >>extends<< Class implements Trait")),
          postInheritanceTypeSpace = Some(SpaceOne(indexOf("Contract HelloWorld extends>> <<Class implements Trait"))),
          headReference = Identifier(indexOf("Contract HelloWorld extends >>Class<< implements Trait"), "Class"),
          tailReferencesOrSpace = Some(Left(SpaceOne(indexOf("Contract HelloWorld extends Class>> <<implements Trait"))))
        )

      /** Assert implements */
      implements shouldBe
        SoftAST.Inheritance(
          index = indexOf("Contract HelloWorld extends Class >>implements Trait<<"),
          inheritanceType = Implements(indexOf("Contract HelloWorld extends Class >>implements<< Trait")),
          postInheritanceTypeSpace = Some(SpaceOne(indexOf("Contract HelloWorld extends Class implements>> <<Trait"))),
          headReference = Identifier(indexOf("Contract HelloWorld extends Class implements >>Trait<<"), "Trait"),
          tailReferencesOrSpace = None
        )
    }

    "implements & extends multiple references" in {
      // Note: The compiler enforces that `extends` be defined before `implements`.
      //       For LSP order is not important.
      val template =
        parseTemplate("Contract MyContract implements A, B, C extends D, E")

      template.inheritance should have size 2
      val implements = template.inheritance.head
      val `extends`  = template.inheritance.last

      /** Assert implements */
      implements shouldBe
        SoftAST.Inheritance(
          index = indexOf("Contract MyContract >>implements A, B, C <<extends D, E"),
          inheritanceType = Implements(indexOf("Contract MyContract >>implements<< A, B, C extends D, E")),
          postInheritanceTypeSpace = Some(SpaceOne(indexOf("Contract MyContract implements>> <<A, B, C extends D, E"))),
          headReference = Identifier(indexOf("Contract MyContract implements >>A<<, B, C extends D, E"), "A"),
          tailReferencesOrSpace = Some(
            Right(
              Seq(
                SoftAST.TailReferences(
                  index = indexOf("Contract MyContract implements A>>, B<<, C extends D, E"),
                  comma = Comma(indexOf("Contract MyContract implements A>>,<< B, C extends D, E")),
                  postCommaSpace = Some(SpaceOne(indexOf("Contract MyContract implements A,>> <<B, C extends D, E"))),
                  reference = Identifier(indexOf("Contract MyContract implements A, >>B<<, C extends D, E"), "B"),
                  postReferenceSpace = None
                ),
                SoftAST.TailReferences(
                  index = indexOf("Contract MyContract implements A, B>>, C <<extends D, E"),
                  comma = Comma(indexOf("Contract MyContract implements A, B>>,<< C extends D, E")),
                  postCommaSpace = Some(SpaceOne(indexOf("Contract MyContract implements A, B,>> <<C extends D, E"))),
                  reference = Identifier(indexOf("Contract MyContract implements A, B, >>C<< extends D, E"), "C"),
                  postReferenceSpace = Some(SpaceOne(indexOf("Contract MyContract implements A, B, C>> <<extends D, E")))
                )
              )
            )
          )
        )

      /** Assert extends */
      `extends` shouldBe
        SoftAST.Inheritance(
          index = indexOf("Contract MyContract implements A, B, C >>extends D, E<<"),
          inheritanceType = Extends(indexOf("Contract MyContract implements A, B, C >>extends<< D, E")),
          postInheritanceTypeSpace = Some(SpaceOne(indexOf("Contract MyContract implements A, B, C extends>> <<D, E"))),
          headReference = Identifier(indexOf("Contract MyContract implements A, B, C extends >>D<<, E"), "D"),
          tailReferencesOrSpace = Some(
            Right(
              Seq(
                SoftAST.TailReferences(
                  index = indexOf("Contract MyContract implements A, B, C extends D>>, E<<"),
                  comma = Comma(indexOf("Contract MyContract implements A, B, C extends D>>,<< E")),
                  postCommaSpace = Some(SpaceOne(indexOf("Contract MyContract implements A, B, C extends D,>> <<E"))),
                  reference = Identifier(indexOf("Contract MyContract implements A, B, C extends D, >>E<<"), "E"),
                  postReferenceSpace = None
                )
              )
            )
          )
        )
    }
  }

  "parse block" when {
    "open brace is missing" in {
      val template =
        parseTemplate("Contract mycontract }")

      template.block.closeCurly shouldBe CloseCurly(indexOf("Contract mycontract >>}<<"))
      template.block.openCurly shouldBe SoftAST.TokenExpected(indexOf("Contract mycontract >><<}"), Token.OpenCurly)
    }

    "close brace and identifier are missing" in {
      val template =
        parseTemplate("Contract {")

      template.identifier shouldBe SoftAST.IdentifierExpected(indexOf("Contract >><<{"))

      template.preIdentifierSpace shouldBe Some(SpaceOne(indexOf("Contract>> <<{")))

      template.block.openCurly shouldBe OpenCurly(indexOf("Contract >>{<<"))
      template.block.closeCurly shouldBe SoftAST.TokenExpected(indexOf("Contract {>><<"), Token.CloseCurly)
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
      template.preIdentifierSpace shouldBe Some(SpaceOne(indexOf("Contract>> <<{")))
      // block
      template.block.openCurly shouldBe OpenCurly(indexOf("Contract >>{<<"))

      template.block.closeCurly shouldBe
        SoftAST.TokenExpected(
          index = indexOf {
            """Contract {
              |  fn function( ->
              |
              |>><<""".stripMargin
          },
          token = Token.CloseCurly
        )

      /**
       * Body part: Function
       */
      template.block.body.parts should have size 1
      val function = template.block.body.parts.head.part.asInstanceOf[SoftAST.Function]

      function.fn shouldBe
        Fn(
          indexOf {
            """Contract {
              |  >>fn<< function( ->
              |
              |""".stripMargin
          }
        )

      function.signature.fnName shouldBe
        Identifier(
          index = indexOf {
            """Contract {
              |  fn >>function<<( ->
              |
              |""".stripMargin
          },
          text = "function"
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
      template.preIdentifierSpace shouldBe Some(SpaceOne(indexOf("Contract>> <<{")))
      // block
      template.block.openCurly shouldBe OpenCurly(indexOf("Contract >>{<<"))

      template.block.closeCurly shouldBe
        SoftAST.TokenExpected(
          index = indexOf {
            """Contract {
              |  TxScript myScript
              |
              |>><<""".stripMargin
          },
          token = Token.CloseCurly
        )

      /**
       * Body part: TxScript
       */
      template.block.body.parts should have size 1
      val txScriptTemplate = template.block.body.parts.head.part.asInstanceOf[SoftAST.Template]

      txScriptTemplate.templateType shouldBe
        TxScript(
          indexOf {
            """Contract {
              |  >>TxScript<< myScript
              |
              |""".stripMargin
          }
        )

      txScriptTemplate.identifier shouldBe
        Identifier(
          index = indexOf {
            """Contract {
              |  TxScript >>myScript<<
              |
              |""".stripMargin
          },
          text = "myScript"
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
        Identifier(
          index = indexOf {
            """Contract MyContract {
              |  >>blah<<
              |
              |""".stripMargin
          },
          text = "blah"
        )

    }
  }

}
