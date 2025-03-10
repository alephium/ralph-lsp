// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEventIdUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "event has no usage" in {
      goToReferences() {
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
      }
    }
  }

  "return non-empty" when {
    "an event usage exists" in {
      goToReferencesForAll(">>Transfer<<".r, ">>Transfe@@r<<")(
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
      goToReferencesForAll(">>Transfer<<".r, ">>Transfe@@r<<")(
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

    "there is inheritance" in {
      goToReferencesForAll(">>Transfer<<".r, ">>Transfe@@r<<")(
        """
          |Abstract Contract Parent() {
          |
          |  event Transfe@@r(to: Address, amount: U256)
          |
          |  fn function0() -> () {
          |    emit >>Transfer<<(to, amount)
          |  }
          |}
          |
          |Contract Parent1() extends Parent() {
          |
          |  pub fn function1() -> () {
          |    emit >>Transfer<<(to, amount)
          |  }
          |}
          |
          |Contract Child() extends Parent1() {
          |
          |  pub fn function2() -> () {
          |    emit >>Transfer<<(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
