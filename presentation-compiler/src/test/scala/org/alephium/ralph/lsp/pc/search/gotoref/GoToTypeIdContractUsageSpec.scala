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
      "within itself" when {
        "strict parseable" in {
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

        "soft parseable" in {
          goToReferencesSoftForAll(">>This<<".r, ">>Thi@@s<<")(
            """
              |Contract This@@(
              |                 this: >>This<<)
              |                 extends >>This<<() {
              |
              |  >>This<<
              |
              |  fn main(param: >>This<<) -> () {
              |    >>This<<
              |    >>This<<.encodeFields!()
              |  }
              |
              |  >>This<<
              |}
              |""".stripMargin
          )
        }
      }

      "within another Contract" when {
        "strict parseable" in {
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

        "soft parseable" in {
          goToReferencesSoftForAll(">>This<<".r, ">>Thi@@s<<")(
            """
              |Contract This@@ {
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
              |  >>This<<
              |
              |  fn main(: >>This<<) {
              |    >>This<<
              |    >>This<<.encodeFields!()
              |    let this = >>This<<.encodeFields!()
              |  }
              |
              |  >>This<<
              |
              |}
              |""".stripMargin
          )
        }
      }
    }
  }

}
