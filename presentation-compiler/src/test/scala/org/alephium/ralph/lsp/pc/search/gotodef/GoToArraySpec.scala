// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class GoToArraySpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there is no array definition" in {
      goToDefinition()(
        """
          |Contract Test()  {
          |  fn main() -> () {
          |    let head = arra@@y[0]
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "there is a single array definition" when {
      "strict" in {
        goToDefinition()(
          """
            |Contract Test(>>array<<: [U256; 2])  {
            |  fn main() -> () {
            |    let head = arra@@y[0]
            |  }
            |}
            |""".stripMargin
        )
      }

      "soft" when {
        "no function" in {
          goToDefinition()(
            """
              |Contract Test(>>array<<: [U256; 2])  {
              |  arra@@y[0]
              |}
              |""".stripMargin
          )
        }

        "no contract" in {
          goToDefinition()(
            """
              |let >>array<< = []
              |arra@@y[0]
              |""".stripMargin
          )
        }
      }
    }

    "there are duplicate array definitions" when {
      "without inheritance" when {
        "strict" in {
          goToDefinition()(
            """
              |Contract Test(>>array<<: [U256; 2])  {
              |  fn main(>>array<<: [U256; 2]) -> () {
              |    let head = arra@@y[0]
              |  }
              |}
              |""".stripMargin
          )
        }

        "soft" in {
          goToDefinition()(
            """
              |Contract Test(>>array<<: [U256; 2])  {
              |
              |  let >>array<< = [0, 1]
              |
              |  fn main(>>array<<: [U256; 2]) {
              |    let >>array<< = [0, 1]
              |    let head = arra@@y[0]
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "within inheritance" when {
        "single source-file" in {
          goToDefinition()(
            """
              |Contract Parent(>>array<<: [U256; 2])  {
              |  fn main(array: [U256; 2]) -> () {
              |    let head = array[0]
              |  }
              |}
              |
              |Contract Test(>>array<<: [U256; 2]) extends Parent(array) {
              |  fn main(>>array<<: [U256; 2]) -> () {
              |    let head = arra@@y[0]
              |  }
              |}
              |""".stripMargin
          )
        }

        "multiple source-files" in {
          goToDefinition()(
            """
              |Contract Parent(>>array<<: [U256; 2])  {
              |  fn main(array: [U256; 2]) -> () {
              |    let head = array[0]
              |  }
              |}
              |""".stripMargin,
            """
              |Contract Test(>>array<<: [U256; 2]) extends Parent(array) {
              |  fn main(>>array<<: [U256; 2]) -> () {
              |    let head = arra@@y[0]
              |  }
              |}
              |""".stripMargin
          )
        }
      }
    }

  }

}
