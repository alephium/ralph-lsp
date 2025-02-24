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
            expressionLeft = SoftAST.ExpressionExpected(indexOf(">><<: Type")),
            preColonSpace = None,
            colon = Colon(indexOf(">>:<< Type")),
            postColonSpace = Some(SpaceOne(indexOf(":>> <<Type"))),
            expressionRight = Identifier(indexOf(": >>Type<<"), "Type")
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
                at = At(indexOf(">>@<<using : Type")),
                preIdentifierSpace = None,
                identifier = Identifier(indexOf("@>>using<< : Type"), "using"),
                postIdentifierSpace = Some(SpaceOne(indexOf("@using>> <<: Type"))),
                tuple = None,
                postTupleSpace = None
              )
            ),
            expressionLeft = SoftAST.ExpressionExpected(indexOf("@using >><<: Type")),
            preColonSpace = None,
            colon = Colon(indexOf("@using >>:<< Type")),
            postColonSpace = Some(SpaceOne(indexOf("@using :>> <<Type"))),
            expressionRight = Identifier(indexOf("@using : >>Type<<"), "Type")
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
            expressionLeft = Identifier(indexOf(">>name<< : Type"), "name"),
            preColonSpace = Some(SpaceOne(indexOf("name>> <<: Type"))),
            colon = Colon(indexOf("name >>:<< Type")),
            postColonSpace = Some(SpaceOne(indexOf("name :>> <<Type"))),
            expressionRight = Identifier(indexOf("name : >>Type<<"), "Type")
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
                at = At(indexOf(">>@<<using name : Type")),
                preIdentifierSpace = None,
                identifier = Identifier(indexOf("@>>using<< name : Type"), "using"),
                postIdentifierSpace = Some(SpaceOne(indexOf("@using>> <<name : Type"))),
                tuple = None,
                postTupleSpace = None
              )
            ),
            expressionLeft = Identifier(indexOf("@using >>name<< : Type"), "name"),
            preColonSpace = Some(SpaceOne(indexOf("@using name>> <<: Type"))),
            colon = Colon(indexOf("@using name >>:<< Type")),
            postColonSpace = Some(SpaceOne(indexOf("@using name :>> <<Type"))),
            expressionRight = Identifier(indexOf("@using name : >>Type<<"), "Type")
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
                at = At(indexOf(">>@<<using @another name : Type")),
                preIdentifierSpace = None,
                identifier = Identifier(indexOf("@>>using<< @another name : Type"), "using"),
                postIdentifierSpace = Some(SpaceOne(indexOf("@using>> <<@another name : Type"))),
                tuple = None,
                postTupleSpace = None
              ),
              SoftAST.Annotation(
                index = indexOf("@using >>@another <<name : Type"),
                at = At(indexOf("@using >>@<<another name : Type")),
                preIdentifierSpace = None,
                identifier = Identifier(indexOf("@using @>>another<< name : Type"), "another"),
                postIdentifierSpace = Some(SpaceOne(indexOf("@using @another>> <<name : Type"))),
                tuple = None,
                postTupleSpace = None
              )
            ),
            expressionLeft = Identifier(indexOf("@using @another >>name<< : Type"), "name"),
            preColonSpace = Some(SpaceOne(indexOf("@using @another name>> <<: Type"))),
            colon = Colon(indexOf("@using @another name >>:<< Type")),
            postColonSpace = Some(SpaceOne(indexOf("@using @another name :>> <<Type"))),
            expressionRight = Identifier(indexOf("@using @another name : >>Type<<"), "Type")
          )
      }
    }
  }

}
