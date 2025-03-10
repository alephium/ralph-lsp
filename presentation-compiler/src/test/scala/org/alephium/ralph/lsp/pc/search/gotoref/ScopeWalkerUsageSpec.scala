// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Inverse test for [[org.alephium.ralph.lsp.pc.search.gotodef.ScopeWalkerSpec]]
 * that implements the same tests, but for references.
 */
class ScopeWalkerUsageSpec extends AnyWordSpec with Matchers {

  "allow variable access" when {
    "defined outside the scope of a block" when {
      "defined before usage" in {
        goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
          """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    let variab@@le = 1
              |    while (true) {
              |      let copyVar = >>variable<<
              |    }
              |    let variable = 1
              |  }
              |
              |}
              |""".stripMargin
        )
      }

      "defined after usage" should {
        "go-to the first definition" in {
          goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
            """
                |Contract Test() {
                |
                |  pub fn test() -> () {
                |    while (true) {
                |      let copyVar = >>variable<<
                |    }
                |    let variab@@le = 1
                |    let variable = 1
                |  }
                |
                |}
                |""".stripMargin
          )
        }

        "prioritise local definition over outside definition" in {
          goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      let variabl@@e = 1
              |      let copyVar = >>variable<<
              |    }
              |    let variable = 1
              |    let variable = 1
              |  }
              |
              |}
              |""".stripMargin
          )
        }
      }

      "defined in a for loop" in {
        goToReferencesForAll(">>index<<".r, ">>inde@@x<<")(
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      for (let mut inde@@x = 0;
            |                   >>index<< < 2;
            |                   >>index<< =
            |                             >>index<< + 1) {
            |        let copyIndex = >>index<<
            |      }
            |      // this not in the search result
            |      let copyIndex = index
            |    }
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }

    "defined within one of the nested blocks and accessed in another" when {
      "define before usage" in {
        goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
          """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      let variab@@le = 1
              |      while (true) {
              |        let b = inner2
              |        for (let mut index = 0; index < 2; index = >>variable<< + 1) {
              |          while (true) {
              |            if (true) {
              |              let copyVar = >>variable<<
              |            }
              |          }
              |        }
              |      }
              |    }
              |  }
              |
              |}
              |""".stripMargin
        )
      }

      "define after usage" in {
        goToReferencesForAll(">>variable<<".r, ">>variabl@@e<<")(
          """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      while (true) {
              |        let b = inner2
              |        while (true) {
              |          while (true) {
              |            let copyVar = >>variable<<
              |          }
              |        }
              |      }
              |      let variabl@@e = 1
              |      let variable = 1
              |    }
              |  }
              |
              |}
              |""".stripMargin
        )
      }
    }

  }

  "disallow variable access" when {
    "definition is in a different scope" when {
      "defined before usage" in {
        goToReferences() {
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while(true) {
            |      let variab@@le = 1
            |    }
            |    while (true) {
            |      let copyVar = variable
            |    }
            |  }
            |
            |}
            |""".stripMargin
        }
      }

      "defined after usage" in {
        goToReferences() {
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      let copyVar = variable
            |    }
            |    for (let mut index = 0; index < 2; index = index + 1) {
            |      let varia@@ble = 1
            |    }
            |  }
            |
            |}
            |""".stripMargin
        }
      }

      "defined after usage but in an inner scope" in {
        goToReferences() {
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      let copyVar = variable
            |      for (let mut index = 0; index < 2; index = index + 1) {
            |        let varia@@ble = 1
            |      }
            |    }
            |  }
            |
            |}
            |""".stripMargin
        }
      }

      "defined in a for loop" in {
        goToReferencesForAll(">>index<<".r, ">>inde@@x<<")(
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      let copyIndex = index
            |      for (let mut ind@@ex = 0;
            |                   >>index<< < 2;
            |                   >>index<< =
            |                              >>index<< + 1) {
            |        let variable = 1
            |      }
            |    }
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }
  }

}
