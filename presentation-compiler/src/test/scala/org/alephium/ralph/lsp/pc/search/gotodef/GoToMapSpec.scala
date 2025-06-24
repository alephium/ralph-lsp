// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToMapSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "map does not exist" in {
      goToDefinition()(
        """
          |Contract Test() {
          |
          |  pub fn function() -> () {
          |    let value = counter@@s[key]
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return self" when {
    "map definition itself is selected" in {
      goToDefinition()(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] >>coun@@ters<<
          |}
          |""".stripMargin
      )
    }

    "duplicate maps exist" when {
      "first map is selected" in {
        goToDefinition()(
          """
            |Abstract Contract Parent() {
            |  mapping[Address, U256] >>coun@@ters<<
            |  mapping[Address, U256] counters
            |}
            |""".stripMargin
        )
      }

      "second map is selected" in {
        goToDefinition()(
          """
            |Abstract Contract Parent() {
            |  mapping[Address, U256] counters
            |  mapping[Address, U256] >>coun@@ters<<
            |}
            |""".stripMargin
        )
      }
    }
  }

  "return non-empty" when {
    "map value is extracted" in {
      goToDefinition()(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] >>counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  mapping[Address, U256] >>counters<<
          |
          |  pub fn function() -> () {
          |    let value = counter@@s[key]
          |  }
          |}
          |""".stripMargin
      )
    }

    "map value is set" in {
      goToDefinition()(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] >>counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  mapping[Address, U256] >>counters<<
          |
          |  pub fn function() -> () {
          |    counter@@s[key] = value + 1
          |  }
          |}
          |""".stripMargin
      )
    }

    "map is inserted" in {
      goToDefinition()(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] >>counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  mapping[Address, U256] >>counters<<
          |
          |  pub fn function() -> () {
          |    counter@@s.insert!(depositor, key, 0)
          |  }
          |}
          |""".stripMargin
      )
    }

    "map item is remove" in {
      goToDefinition()(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] >>counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  mapping[Address, U256] >>counters<<
          |
          |  pub fn function() -> () {
          |    counter@@s.remove!(depositRecipient, key)
          |  }
          |}
          |""".stripMargin
      )
    }

    "map is checked for contains" in {
      goToDefinition()(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] >>counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  mapping[Address, U256] >>counters<<
          |
          |  pub fn function() -> () {
          |    counter@@s.contains!(callerAddress!())
          |  }
          |}
          |""".stripMargin
      )
    }

    "map function is returned" in {
      goToDefinition()(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] >>counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  mapping[Address, U256] >>counters<<
          |
          |  pub fn function() -> Bool {
          |    return counter@@s.contains!(callerAddress!())
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
