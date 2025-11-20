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
    "map has usages" when {
      "strict parseable" in {
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

      "soft parseable" when {
        def doTest(definition: String) =
          goToReferencesSoftForAll(">>counters<<".r, ">>counter@@s<<")(
            s"""
              |Abstract Contract Parent {
              |  $definition
              |}
              |
              |Contract Child extends Parent {
              |
              |  fn function -> Boolean {
              |    >>counters<<
              |    let value = >>counters<<[key]
              |    >>counters<<[key] = value + 1
              |    >>counters<<.insert!(depositor, key, 0)
              |    >>counters<<.remove!(depositRecipient, key)
              |    return >>counters<<.contains!(callerAddress!())
              |  }
              |
              |  >>counters<<
              |  let value = >>counters<<[key]
              |  >>counters<<[key] = value + 1
              |  >>counters<<.insert!(depositor, key, 0)
              |  >>counters<<.remove!(depositRecipient, key)
              |  return >>counters<<.contains!(callerAddress!())
              |}
              |""".stripMargin
          )

        "value type is not defined" in {
          doTest("mapping[Address, ] counter@@s")
        }

        "key type is not defined" in {
          doTest("mapping[, Value] counter@@s")
        }

        "key and value types are not defined" in {
          doTest("mapping[ , ] counter@@s")
        }

        "types params are not defined" in {
          doTest("mapping counter@@s")
        }
      }
    }
  }

}
