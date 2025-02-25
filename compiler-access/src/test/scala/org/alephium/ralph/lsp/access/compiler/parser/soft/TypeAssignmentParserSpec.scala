package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TypeAssignmentParserSpec extends AnyWordSpec {

  "report error" when {
    "type name is not supplied" when {
      "without annotation" in {
        val assignment =
          parseTypeAssignment(": Type")

        assignment shouldBe
          SoftAST.TypeAssignment(
            index = indexOf(">>: Type<<"),
            annotations = Seq.empty,
            expressionLeft = ExpressionExpected(">><<: Type"),
            preColonSpace = None,
            colon = Colon(">>:<< Type"),
            postColonSpace = Some(Space(":>> <<Type")),
            expressionRight = Identifier(": >>Type<<")
          )
      }

      "with annotation" in {
        val assignment =
          parseTypeAssignment("@using : Type")

        assignment shouldBe
          SoftAST.TypeAssignment(
            index = indexOf(">>@using : Type<<"),
            annotations = Seq(
              SoftAST.Annotation(
                index = indexOf(">>@using <<: Type"),
                at = At(">>@<<using : Type"),
                preIdentifierSpace = None,
                identifier = Identifier("@>>using<< : Type"),
                postIdentifierSpace = Some(Space("@using>> <<: Type")),
                tuple = None,
                postTupleSpace = None
              )
            ),
            expressionLeft = ExpressionExpected("@using >><<: Type"),
            preColonSpace = None,
            colon = Colon("@using >>:<< Type"),
            postColonSpace = Some(Space("@using :>> <<Type")),
            expressionRight = Identifier("@using : >>Type<<")
          )
      }
    }
  }

  "success" when {
    "type assignment is fully defined" when {
      "without annotation" in {
        val assignment =
          parseTypeAssignment("name : Type")

        assignment shouldBe
          SoftAST.TypeAssignment(
            index = indexOf(">>name : Type<<"),
            annotations = Seq.empty,
            expressionLeft = Identifier(">>name<< : Type"),
            preColonSpace = Some(Space("name>> <<: Type")),
            colon = Colon("name >>:<< Type"),
            postColonSpace = Some(Space("name :>> <<Type")),
            expressionRight = Identifier("name : >>Type<<")
          )
      }

      "with annotation" in {
        val assignment =
          parseTypeAssignment("@using name : Type")

        assignment shouldBe
          SoftAST.TypeAssignment(
            index = indexOf(">>@using name : Type<<"),
            annotations = Seq(
              SoftAST.Annotation(
                index = indexOf(">>@using <<name : Type"),
                at = At(">>@<<using name : Type"),
                preIdentifierSpace = None,
                identifier = Identifier("@>>using<< name : Type"),
                postIdentifierSpace = Some(Space("@using>> <<name : Type")),
                tuple = None,
                postTupleSpace = None
              )
            ),
            expressionLeft = Identifier("@using >>name<< : Type"),
            preColonSpace = Some(Space("@using name>> <<: Type")),
            colon = Colon("@using name >>:<< Type"),
            postColonSpace = Some(Space("@using name :>> <<Type")),
            expressionRight = Identifier("@using name : >>Type<<")
          )
      }

      "with multiple annotations" in {
        val assignment =
          parseTypeAssignment("@using @another name : Type")

        assignment shouldBe
          SoftAST.TypeAssignment(
            index = indexOf(">>@using @another name : Type<<"),
            annotations = Seq(
              SoftAST.Annotation(
                index = indexOf(">>@using <<@another name : Type"),
                at = At(">>@<<using @another name : Type"),
                preIdentifierSpace = None,
                identifier = Identifier("@>>using<< @another name : Type"),
                postIdentifierSpace = Some(Space("@using>> <<@another name : Type")),
                tuple = None,
                postTupleSpace = None
              ),
              SoftAST.Annotation(
                index = indexOf("@using >>@another <<name : Type"),
                at = At("@using >>@<<another name : Type"),
                preIdentifierSpace = None,
                identifier = Identifier("@using @>>another<< name : Type"),
                postIdentifierSpace = Some(Space("@using @another>> <<name : Type")),
                tuple = None,
                postTupleSpace = None
              )
            ),
            expressionLeft = Identifier("@using @another >>name<< : Type"),
            preColonSpace = Some(Space("@using @another name>> <<: Type")),
            colon = Colon("@using @another name >>:<< Type"),
            postColonSpace = Some(Space("@using @another name :>> <<Type")),
            expressionRight = Identifier("@using @another name : >>Type<<")
          )
      }
    }
  }

}
