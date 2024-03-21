package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEventSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "event does not exists" in {
      goTo(
        """
          |Contract Test() {
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "an event exists" in {
      goTo(
        """
          |Contract Test() {
          |
          |  >>event Transfer(to: Address, amount: U256)<<
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }

    "multiple events exist" in {
      goTo(
        """
          |Contract Test() {
          |
          |  >>event Transfer(to: Address, amount: U256)<<
          |  >>event Transfer(amount: U256)<<
          |  >>event Transfer(to: Address)<<
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }

    "events exist with duplicate names" in {
      goTo(
        """
          |Contract Transfer(transfer: Transfer) {
          |
          |  >>event Transfer(to: Address, amount: U256)<<
          |  >>event Transfer(amount: U256)<<
          |  >>event Transfer(to: Address)<<
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
