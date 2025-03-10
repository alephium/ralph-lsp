// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToMapUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there is no map usage" in {
      goToReferences() {
        """
          |Contract Test() {
          |
          |  mapping[Address, U256] counter@@s
          |
          |  pub fn function() -> () { }
          |}
          |""".stripMargin
      }
    }
  }

  "return non-empty" when {
    "map has usages" in {
      goToReferencesForAll(">>counters<<".r, ">>counter@@s<<")(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] counter@@s
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  pub fn function() -> Boolean {
          |    let value = >>counters<<[key]
          |    >>counters<<[key] = value + 1
          |    >>counters<<.insert!(depositor, key, 0)
          |    >>counters<<.remove!(depositRecipient, key)
          |    return >>counters<<.contains!(callerAddress!())
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
