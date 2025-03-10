// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEventSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "event does not exists" in {
      goToDefinitionStrict()(
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

  "return self" when {
    "an event definition is selected" in {
      goToDefinitionStrict()(
        """
          |Contract Test() extends Parent() {
          |
          |  event >>Transf@@er<<(to: Address, amount: U256)
          |
          |  pub fn function() -> () { }
          |}
          |""".stripMargin
      )
    }

    "duplicate event definitions exist" in {
      goToDefinitionStrict()(
        """
          |Contract Test() extends Parent() {
          |
          |  event Transfer(to: Address, amount: U256)
          |  event >>Transf@@er<<(to: Address, amount: U256)
          |
          |  pub fn function() -> () { }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "an event exists" in {
      goToDefinitionStrict()(
        """
          |Abstract Contract Parent() {
          |
          |  event TransferNotUsed(to: Address, amount: U256)
          |
          |  event >>Transfer<<(to: Address, amount: U256)
          |
          |}
          |
          |Contract Test() extends Parent() {
          |
          |  event >>Transfer<<(to: Address, amount: U256)
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }

    "duplicate events exist" in {
      goToDefinitionStrict()(
        """
          |Abstract Contract Parent() {
          |
          |  event >>Transfer<<(amount: U256)
          |  event >>Transfer<<(to: Address, amount: U256)
          |  event TransferNotUsed(to: Address, amount: U256)
          |  event >>Transfer<<(to: Address)
          |
          |}
          |
          |Contract Test() extends Parent() {
          |
          |  event >>Transfer<<(to: Address, amount: U256)
          |  event >>Transfer<<(amount: U256)
          |  event >>Transfer<<(to: Address)
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
