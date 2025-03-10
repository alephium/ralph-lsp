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
    "there is a single array definition" in {
      goToDefinitionStrict()(
        """
          |Contract Test(>>array<<: [U256; 2])  {
          |  fn main() -> () {
          |    let head = arra@@y[0]
          |  }
          |}
          |""".stripMargin
      )
    }

    "there are duplicate array definitions" when {
      "without inheritance" in {
        goToDefinitionStrict()(
          """
            |Contract Test(>>array<<: [U256; 2])  {
            |  fn main(>>array<<: [U256; 2]) -> () {
            |    let head = arra@@y[0]
            |  }
            |}
            |""".stripMargin
        )
      }

      "within inheritance" in {
        goToDefinitionStrict()(
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
    }

  }

}
