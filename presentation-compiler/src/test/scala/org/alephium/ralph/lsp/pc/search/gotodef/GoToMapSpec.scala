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

    "soft parseable (contains syntax errors - not strict parseable)" when {
      "duplicates exist" when {
        "first map is selected" in {
          goToDefinitionSoft() {
            """
              |mapping[U256, U256] >>ma@@p<<
              |mapping[U256, U256] map
              |""".stripMargin
          }
        }

        "second map is selected" in {
          goToDefinitionSoft() {
            """
              |mapping[U256, U256] map
              |mapping[U256, U256] >>ma@@p<<
              |""".stripMargin
          }
        }
      }

      "types are not provided" when {
        "first map is selected" in {
          goToDefinitionSoft() {
            """
              |mapping[] >>ma@@p<<
              |mapping[] map
              |""".stripMargin
          }
        }

        "second map is selected" in {
          goToDefinitionSoft() {
            """
              |mapping[] map
              |mapping[] >>ma@@p<<
              |""".stripMargin
          }
        }
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

    "soft parseable (contains syntax errors - not strict parseable)" when {
      "the map is within" when {
        "contract block" in {
          goToDefinitionSoft() {
            """
              |Contract Main {
              |  mapping[Blah, ] >>map<<
              |  ma@@p
              |""".stripMargin
          }
        }

        "function block" in {
          goToDefinitionSoft() {
            """
              |Contract Main {
              |  mapping >>map<<
              |
              |  fn main {
              |    mapping[] >>map<<
              |    ma@@p
              |  }
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

}
