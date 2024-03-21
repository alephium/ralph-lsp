package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEventIdUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "event has no usage" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfe@@r(to: Address, amount: U256)
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

  "return non-empty" when {
    "an event usage exists" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfe@@r(to: Address, amount: U256)
          |
          |  pub fn function() -> () {
          |    emit >>Transfer<<(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }

    "multiple usage exists" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfe@@r(to: Address, amount: U256)
          |
          |  pub fn function() -> () {
          |    emit >>Transfer<<(to, amount)
          |    for (let mut index = 0; index <= 4; index = index + 1) {
          |      emit >>Transfer<<(to, amount)
          |    }
          |    emit >>Transfer<<(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
