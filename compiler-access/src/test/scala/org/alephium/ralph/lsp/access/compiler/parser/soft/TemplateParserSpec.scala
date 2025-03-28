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

class TemplateParserSpec extends AnyWordSpec with Matchers {

  "parse template type" when {
    def testTokenIsReportedAsIdentifier(templateToken: String) = {
      val block = parseSoft(templateToken)

      block.parts should have size 1

      block.parts.head shouldBe
        Identifier(
          index = indexOf(s">>$templateToken<<"),
          text = templateToken
        )
    }

    "Contract" when {
      "camelcase" in {
        val templateToken =
          "Contract"

        val template =
          parseTemplate(templateToken)

        template.index shouldBe indexOf(s">>$templateToken<<")
        template.templateType shouldBe Contract(s">>$templateToken<<")
        template.preIdentifierSpace shouldBe None
        template.identifier shouldBe IdentifierExpected(s"$templateToken>><<")
        template.preParamSpace shouldBe None
        template.params shouldBe empty
        template.postParamSpace shouldBe None
        template.block shouldBe None
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
        template.templateType shouldBe TxScript(s">>$templateToken<<")
        template.preIdentifierSpace shouldBe None
        template.identifier shouldBe IdentifierExpected(s"$templateToken>><<")
        template.preParamSpace shouldBe empty
        template.params shouldBe empty
        template.postParamSpace shouldBe empty
        template.block shouldBe None
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
      params.openToken.value shouldBe OpenParen("Contract mycontract>>(<<")
      params.closeToken.value shouldBe SoftAST.TokenExpected(indexOf("Contract mycontract(>><<"), Token.CloseParen)
    }

    "params are empty" in {
      val template =
        parseTemplate("Contract mycontract( )")

      val params = template.params.value

      params.preHeadExpressionSpace.value shouldBe Space("Contract mycontract(>> <<)")
    }
  }

  "parse inheritance" when {
    "implements without defining implementing class" in {
      val template =
        parseTemplate("Contract MyContract implements")

      template.inheritance should contain only
        SoftAST.Inheritance(
          index = indexOf("Contract MyContract >>implements<<"),
          inheritanceType = Implements("Contract MyContract >>implements<<"),
          postInheritanceTypeSpace = None,
          headReference = IdentifierExpected("Contract MyContract implements>><<"),
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
          inheritanceType = Extends("Contract HelloWorld >>extends<< Class implements Trait"),
          postInheritanceTypeSpace = Some(Space("Contract HelloWorld extends>> <<Class implements Trait")),
          headReference = Identifier("Contract HelloWorld extends >>Class<< implements Trait"),
          tailReferencesOrSpace = Some(Left(Space("Contract HelloWorld extends Class>> <<implements Trait")))
        )

      /** Assert implements */
      implements shouldBe
        SoftAST.Inheritance(
          index = indexOf("Contract HelloWorld extends Class >>implements Trait<<"),
          inheritanceType = Implements("Contract HelloWorld extends Class >>implements<< Trait"),
          postInheritanceTypeSpace = Some(Space("Contract HelloWorld extends Class implements>> <<Trait")),
          headReference = Identifier("Contract HelloWorld extends Class implements >>Trait<<"),
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
          inheritanceType = Implements("Contract MyContract >>implements<< A, B, C extends D, E"),
          postInheritanceTypeSpace = Some(Space("Contract MyContract implements>> <<A, B, C extends D, E")),
          headReference = Identifier("Contract MyContract implements >>A<<, B, C extends D, E"),
          tailReferencesOrSpace = Some(
            Right(
              Seq(
                SoftAST.TailReferences(
                  index = indexOf("Contract MyContract implements A>>, B<<, C extends D, E"),
                  comma = Comma("Contract MyContract implements A>>,<< B, C extends D, E"),
                  postCommaSpace = Some(Space("Contract MyContract implements A,>> <<B, C extends D, E")),
                  reference = Identifier("Contract MyContract implements A, >>B<<, C extends D, E"),
                  postReferenceSpace = None
                ),
                SoftAST.TailReferences(
                  index = indexOf("Contract MyContract implements A, B>>, C <<extends D, E"),
                  comma = Comma("Contract MyContract implements A, B>>,<< C extends D, E"),
                  postCommaSpace = Some(Space("Contract MyContract implements A, B,>> <<C extends D, E")),
                  reference = Identifier("Contract MyContract implements A, B, >>C<< extends D, E"),
                  postReferenceSpace = Some(Space("Contract MyContract implements A, B, C>> <<extends D, E"))
                )
              )
            )
          )
        )

      /** Assert extends */
      `extends` shouldBe
        SoftAST.Inheritance(
          index = indexOf("Contract MyContract implements A, B, C >>extends D, E<<"),
          inheritanceType = Extends("Contract MyContract implements A, B, C >>extends<< D, E"),
          postInheritanceTypeSpace = Some(Space("Contract MyContract implements A, B, C extends>> <<D, E")),
          headReference = Identifier("Contract MyContract implements A, B, C extends >>D<<, E"),
          tailReferencesOrSpace = Some(
            Right(
              Seq(
                SoftAST.TailReferences(
                  index = indexOf("Contract MyContract implements A, B, C extends D>>, E<<"),
                  comma = Comma("Contract MyContract implements A, B, C extends D>>,<< E"),
                  postCommaSpace = Some(Space("Contract MyContract implements A, B, C extends D,>> <<E")),
                  reference = Identifier("Contract MyContract implements A, B, C extends D, >>E<<"),
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
      val root =
        parseSoft("Contract mycontract }")

      root.parts should have size 2

      val template = root.parts.head.asInstanceOf[SoftAST.Template]
      template.block shouldBe None

      val unresolved = root.parts.last.asInstanceOf[SoftAST.Unresolved]
      unresolved shouldBe
        Unresolved(
          index = indexOf("Contract mycontract >>}<<"),
          text = "}"
        )
    }

    "close brace and identifier are missing" in {
      val template =
        parseTemplate("Contract {")

      template.identifier shouldBe IdentifierExpected("Contract >><<{")

      template.preIdentifierSpace shouldBe Some(Space("Contract>> <<{"))

      template.block.value.openCurly shouldBe OpenCurly("Contract >>{<<")
      template.block.value.closeCurly shouldBe SoftAST.TokenExpected(indexOf("Contract {>><<"), Token.CloseCurly)
    }

    "a function is defined" in {
      val template =
        parseTemplate {
          """Contract {
            |  fn function( ->
            |
            |""".stripMargin
        }

      template.identifier shouldBe IdentifierExpected("Contract >><<{")
      template.preIdentifierSpace shouldBe Some(Space("Contract>> <<{"))
      // block
      template.block.value.openCurly shouldBe OpenCurly("Contract >>{<<")

      template.block.value.closeCurly shouldBe
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
       * Block part: Function
       */
      val parts = template.block.value.partsNonEmpty
      parts should have size 1

      val function = parts.head.asInstanceOf[SoftAST.Function]

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

      template.identifier shouldBe IdentifierExpected("Contract >><<{")
      template.preIdentifierSpace shouldBe Some(Space("Contract>> <<{"))
      // block
      template.block.value.openCurly shouldBe OpenCurly("Contract >>{<<")

      template.block.value.closeCurly shouldBe
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
       * Block part: TxScript
       */
      val parts = template.block.value.partsNonEmpty
      parts should have size 1

      val txScriptTemplate = parts.head.asInstanceOf[SoftAST.Template]

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
       * Block part: TxScript
       */
      val parts = template.block.value.partsNonEmpty
      parts should have size 1

      val part = parts.head

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
