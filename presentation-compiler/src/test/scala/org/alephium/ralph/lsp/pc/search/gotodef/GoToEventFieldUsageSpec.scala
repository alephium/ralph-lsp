package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEventFieldUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "first event field has no usage" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfer(t@@o: Address, amount: U256)
          |
          |  pub fn function() -> () {
          |
          |  }
          |}
          |
          |""".stripMargin
      )
    }

    "second event field has no usage" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
          |
          |  pub fn function() -> () {
          |
          |  }
          |}
          |
          |""".stripMargin
      )
    }
  }

  "return non-empty for an event field" when {
    "it has a usage" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
          |
          |  pub fn function() -> () {
          |    emit Transfer(to, >>someAmount<<)
          |  }
          |}
          |""".stripMargin
      )
    }

    "it has multiple usages" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
          |
          |  pub fn function() -> () {
          |    emit Transfer(to, >>amount1<<)
          |    for (let mut index = 0; index <= 4; index = index + 1) {
          |      emit Transfer(to, >>amount2<<)
          |    }
          |    emit Transfer(to, >>amount3<<)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
