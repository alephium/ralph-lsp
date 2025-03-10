// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class GoToArrayUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there is no array usage" in {
      goToReferences() {
        """
          |Contract Test(arra@@y: [U256; 2])  {
          |  fn main(array: [U256; 2]) -> () {
          |
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "return non-empty" when {
    "there is a single array definition" in {
      goToReferencesForAll(">>array<<".r, ">>arra@@y<<")(
        """
          |Contract Test(arra@@y: [U256; 2])  {
          |  fn main() -> () {
          |    let head = >>array<<[0]
          |  }
          |}
          |""".stripMargin
      )
    }

    "there are multiple usages" when {
      "without inheritance" in {
        goToReferencesForAll(">>array<<".r, ">>arra@@y<<")(
          """
            |Contract Test(arra@@y: [U256; 2])  {
            |  fn main() -> () {
            |    let head = >>array<<[0]
            |    for (let mut index = >>array<<[0];
            |                 index <= >>array<<[1];
            |                 index = index + 1) {
            |      let bool = true
            |    }
            |  }
            |}
            |""".stripMargin
        )
      }

      "within inheritance" in {
        goToReferencesForAll(">>array<<".r, ">>arra@@y<<")(
          """
              |Contract Parent(arra@@y: [U256; 2])  {
              |  fn main() -> () {
              |    let head = >>array<<[0]
              |  }
              |}
              |
              |Contract Test() extends Parent(array) {
              |  fn main() -> () {
              |    let head = >>array<<[0]
              |    for (let mut index = >>array<<[0];
              |                 index <= >>array<<[1];
              |                 index = index + 1) {
              |      let bool = true
              |    }
              |  }
              |}
              |""".stripMargin
        )
      }
    }
  }

}
