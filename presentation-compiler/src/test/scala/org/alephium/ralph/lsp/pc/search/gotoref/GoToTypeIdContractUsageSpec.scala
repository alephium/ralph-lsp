// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class GoToTypeIdContractUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "no usage exists" in {
      goToReferences() {
        """
          |Contract This@@() {
          |
          |  fn main() -> () { }
          |
          |}
          |
          |""".stripMargin
      }
    }
  }

  "return non-empty" when {
    "usage exists" when {
      "within itself" in {
        goToReferencesForAll(">>This<<".r, ">>Thi@@s<<")(
          """
            |Contract This@@(
            |                 this: >>This<<)
            |                 extends >>This<<() {
            |
            |  fn main(param: >>This<<) -> () {
            |    let test = >>This<<.encodeFields!()
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "within another Contract" in {
        goToReferencesForAll(">>This<<".r, ">>Thi@@s<<")(
          """
            |Contract This@@() {
            |
            |  fn main(param: Another) -> () {
            |    let another = Another.encodeFields!()
            |  }
            |
            |}
            |
            |Contract Another(this: >>This<<)
            |   extends >>This<<() {
            |
            |  fn main(param: >>This<<) -> () {
            |    let this = >>This<<.encodeFields!()
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }
  }

}
