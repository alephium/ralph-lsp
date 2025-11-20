// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEventFieldUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "first event field has no usage" in {
      goToReferences() {
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
      }
    }

    "second event field has no usage" in {
      goToReferences() {
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
      }
    }

    "includeEventFieldReferences is disabled" in {
      goToReferences(settings = testGoToRefSetting.copy(includeEventFieldReferences = false)) {
        """
          |Contract Test() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
          |
          |  pub fn function() -> () {
          |    emit Transfer(to, someAmount)
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "return non-empty for an event field" when {
    "it has a usage" in {
      goToReferencesStrict() {
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
      }
    }

    "it has multiple usages" in {
      goToReferencesStrict() {
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
      }
    }

    "there is inheritance" in {
      goToReferencesStrict() {
        """
          |Abstract Contract Parent() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
          |
          |  fn function0() -> () {
          |    emit Transfer(to, >>amount1<<)
          |  }
          |}
          |
          |Contract Parent1() extends Parent() {
          |
          |  pub fn function1() -> () {
          |    emit Transfer(to, >>amount1<<)
          |  }
          |}
          |
          |Contract Child() extends Parent1() {
          |
          |  pub fn function2() -> () {
          |    emit Transfer(to, >>amount1<<)
          |  }
          |}
          |""".stripMargin
      }
    }
  }

}
