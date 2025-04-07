// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToEventFieldSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "an event field and an assignment value have duplicate names" when {
      "event is defined external to the contract" in {
        goToDefinitionSoft() {
          """
            |event MyEvent(eventField: Bool)
            |
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let copy = eventFie@@ld
            |  }
            |}
            |""".stripMargin
        }
      }

      "event is defined within the contract" in {
        goToDefinition() {
          """
            |Contract Test() {
            |
            |  event MyEvent(eventField: Bool)
            |
            |  pub fn function() -> () {
            |    let copy = eventFie@@ld
            |  }
            |}
            |""".stripMargin
        }
      }
    }
  }

  "return self" when {
    "the event field is selected" when {
      "soft-parseable" when {
        "valid syntax" in {
          goToDefinitionSoft() {
            """
              |event MyEvent(>>eventFie@@ld<<: Bool)
              |""".stripMargin
          }
        }

        "event name is not defined" in {
          goToDefinitionSoft() {
            """
              |event (>>eventFie@@ld<<: Bool)
              |""".stripMargin
          }
        }

        "event field type is not defined" in {
          goToDefinitionSoft() {
            """
              |event (>>eventFie@@ld<<)
              |""".stripMargin
          }
        }

        "closing paren is not defined" in {
          goToDefinitionSoft() {
            """
              |event (>>eventFie@@ld<<
              |""".stripMargin
          }
        }
      }

      "strict-parseable" in {
        goToDefinition() {
          """
            |Contract Test() {
            |
            |  event MyEvent(>>eventFi@@eld<<: Bool)
            |
            |  fn function() -> () { }
            |}
            |""".stripMargin
        }
      }
    }
  }

}
