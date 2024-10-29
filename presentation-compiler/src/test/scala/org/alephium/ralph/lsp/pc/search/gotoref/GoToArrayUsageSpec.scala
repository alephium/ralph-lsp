// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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
