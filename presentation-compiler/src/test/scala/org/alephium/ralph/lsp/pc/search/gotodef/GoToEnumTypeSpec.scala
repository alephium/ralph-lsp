package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEnumTypeSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "enum type does not exist" in {
      goTo(
        """
          |Contract MyContract() {
          |  pub fn function() -> () {
          |    let go_to_enum = Enum@@Type.Field0
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "user selects the enum type of the first field" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let go_to_enum1 = Enum@@Type.Field0
          |    let go_to_enum2 = EnumType.Field1
          |  }
          |}
          |""".stripMargin
      )
    }

    "user selects the enum type of the second field" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let go_to_enum1 = EnumType.Field0
          |    let go_to_enum2 = EnumTy@@pe.Field1
          |  }
          |}
          |""".stripMargin
      )
    }

    "there are multiple enum types with duplicate names" in {
      goTo(
        """
          |Abstract Contract Parent2() {
          |
          |  enum EnumTypeNotUsed {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |Abstract Contract Parent1() extends Parent2() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  enum EnumTypeNotUsed {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |Contract MyContract() extends Parent1() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let go_to_enum = Enu@@mType.Field0
          |  }
          |}
          |""".stripMargin
      )
    }
  }
}
