package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEnumFieldSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "enum type does not exist" in {
      goTo(
        """
          |Contract MyContract() {
          |  pub fn function() -> () {
          |    let field = EnumType.Fie@@ld0
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "user selects the first enum field" in {
      goTo(
        """
          |// This parent is not inherited
          |Abstract Contract ParentNotUsed() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |
          |Abstract Contract Parent() {
          |
          |  enum EnumType {
          |    >>Field0 = 0<<
          |    Field1 = 1
          |  }
          |}
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum EnumType {
          |    >>Field0 = 0<<
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let field0 = EnumType.Fie@@ld0
          |    let field1 = EnumType.Field1
          |  }
          |}
          |""".stripMargin
      )
    }

    "user selects the second enum field" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    >>Field1 = 1<<
          |  }
          |}
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    >>Field1 = 1<<
          |  }
          |
          |  pub fn function() -> () {
          |    let field0 = EnumType.Field0
          |    let field1 = EnumType.Fie@@ld1
          |  }
          |}
          |""".stripMargin
      )
    }

    "there are duplicate enum types and fields" when {
      "user selects the first enum field" in {
        goTo(
          """
            |Abstract Contract Parent() {
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |}
            |
            |Contract MyContract() extends Parent() {
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Fi@@eld0
            |    let field1 = EnumType.Field1
            |  }
            |}
            |""".stripMargin
        )
      }

      "user selects the second enum field" in {
        goTo(
          """
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Field0 = 0
            |    >>Field1 = 1<<
            |  }
            |
            |  enum EnumType {
            |    Field0 = 0
            |    >>Field1 = 1<<
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0
            |    let field1 = EnumType.Fi@@eld1
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "there are duplicate enum types with distinct fields" when {
      "user selects the first enum field" in {
        goTo(
          """
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    Field2 = 2
            |    Field3 = 3
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Fie@@ld0
            |    let field1 = EnumType.Field1
            |    let field2 = EnumType.Field2
            |    let field3 = EnumType.Field3
            |  }
            |}
            |""".stripMargin
        )
      }

      "user selects the third enum field" in {
        goTo(
          """
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Field0 = 0
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field2 = 2<<
            |    Field3 = 3
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0
            |    let field1 = EnumType.Field1
            |    let field2 = EnumType.Fie@@ld2
            |    let field3 = EnumType.Field3
            |  }
            |}
            |""".stripMargin
        )
      }

      "an enum field is selected that's implemented within a parent" in {
        goTo(
          """
            |Abstract Contract Parent2() {
            |  enum EnumType {
            |    >>Field0 = 00<<
            |    Field3 = 3
            |  }
            |}
            |
            |Abstract Contract Parent1() extends Parent2() {
            |  enum EnumType {
            |    >>Field0 = 00<<
            |    Field3 = 3
            |  }
            |}
            |
            |Contract MyContract() extends Parent1() {
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0@@
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }
}
