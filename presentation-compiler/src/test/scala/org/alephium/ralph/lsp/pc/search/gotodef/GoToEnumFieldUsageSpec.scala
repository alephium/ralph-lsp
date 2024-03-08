package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEnumFieldUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there are no enum field calls or usages" in {
      goTo(
        """
          |Contract MyContract() {
          |
          |  enum EnumType {
          |    Fie@@ld0 = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "there are multiple enum field calls or usages" when {
      "the first field is selected" in {
        goTo(
          """
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Fie@@ld0 = 0
            |    Field1 = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.>>Field0<<
            |    let field1 = EnumType.Field1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.>>Field0<<
            |    for (let mut index = 0; index <= 4; index = index + 1) {
            |      let field1 = EnumType.Field1
            |    }
            |  }
            |}
            |""".stripMargin
        )
      }

      "the second field is selected" in {
        goTo(
          """
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Field0 = 0
            |    Fie@@ld1 = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0
            |    let field1 = EnumType.>>Field1<<
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0
            |    for (let mut index = 0; index <= 4; index = index + 1) {
            |      let field1 = EnumType.>>Field1<<
            |    }
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }
}
