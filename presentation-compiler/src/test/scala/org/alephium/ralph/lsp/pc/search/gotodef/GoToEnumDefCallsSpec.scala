package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEnumDefCallsSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there are no enum calls or usages" in {
      goTo(
        """
          |Contract MyContract() {
          |
          |  enum Enum@@Type {
          |    Field0 = 0
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
    "there are multiple calls or usages" in {
      goTo(
        """
          |Contract MyContract() {
          |
          |  enum Enum@@Type {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let field0 = >>EnumType<<.Field0
          |    let field1 = >>EnumType<<.Field1
          |  }
          |
          |  pub fn function() -> () {
          |    let field0 = >>EnumType<<.Field0
          |    for (let mut index = 0; index <= 4; index = index + 1) {
          |      let field1 = >>EnumType<<.Field1
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }
  }
}
