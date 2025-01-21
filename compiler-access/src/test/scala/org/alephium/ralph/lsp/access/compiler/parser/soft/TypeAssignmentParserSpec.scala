package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TypeAssignmentParserSpec extends AnyWordSpec {

  "report error" when {
    "type name is not supplied" in {
      val assignment =
        parseTypeAssignment(": Type")

      assignment shouldBe
        SoftAST.TypeAssignment(
          index = indexOf(">>: Type<<"),
          name = SoftAST.ExpressionExpected(indexOf(">><<: Type")),
          preColonSpace = None,
          colon = Colon(indexOf(">>:<< Type")),
          postColonSpace = Some(SpaceOne(indexOf(":>> <<Type"))),
          tpe = Identifier(indexOf(": >>Type<<"), "Type")
        )
    }
  }

  "success" when {
    "type assignment is fully defined" when {
      "defined as identifier" in {
        val assignment =
          parseTypeAssignment("name : Type")

        assignment shouldBe
          SoftAST.TypeAssignment(
            index = indexOf(">>name : Type<<"),
            name = Identifier(indexOf(">>name<< : Type"), "name"),
            preColonSpace = Some(SpaceOne(indexOf("name>> <<: Type"))),
            colon = Colon(indexOf("name >>:<< Type")),
            postColonSpace = Some(SpaceOne(indexOf("name :>> <<Type"))),
            tpe = Identifier(indexOf("name : >>Type<<"), "Type")
          )
      }
    }
  }

}
