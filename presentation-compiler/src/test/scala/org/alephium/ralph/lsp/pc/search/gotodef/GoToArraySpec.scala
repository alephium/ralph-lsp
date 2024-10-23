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

    "there are duplicate array definitions" when {
      "without inheritance" in {
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

      "within inheritance" in {
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
    }

  }

}
