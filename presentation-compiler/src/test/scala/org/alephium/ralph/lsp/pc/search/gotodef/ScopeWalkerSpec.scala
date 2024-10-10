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

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Tests go-to definition on scoping rules defined in [[ScopeWalker]] */
class ScopeWalkerSpec extends AnyWordSpec with Matchers {

  "allow variable access" when {
    "defined outside the scope of a block" when {
      "defined before usage" in {
        goToDefinition(
          """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    let >>variable<< = 1
              |    let >>variable<< = 1
              |    while (true) {
              |      let copyVar = variabl@@e
              |    }
              |  }
              |
              |}
              |""".stripMargin
        )
      }

      "defined after usage" should {
        "go-to the first definition" in {
          goToDefinition(
            """
                |Contract Test() {
                |
                |  pub fn test() -> () {
                |    while (true) {
                |      let copyVar = variabl@@e
                |    }
                |    let >>variable<< = 1
                |    let variable = 1
                |  }
                |
                |}
                |""".stripMargin
          )
        }

        "prioritise local definition over outside definition" in {
          goToDefinition(
            """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      let >>variable<< = 1
              |      let copyVar = variabl@@e
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
        goToDefinition(
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      for (let mut >>index<< = 0; index < 2; index = index + 1) {
            |        let copyIndex = inde@@x
            |      }
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
        goToDefinition(
          """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      let >>variable<< = 1
              |      let >>variable<< = 1
              |      while (true) {
              |        let b = inner2
              |        for (let mut index = 0; index < 2; index = index + 1) {
              |          while (true) {
              |            if (true) {
              |              let copyVar = variabl@@e
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
        goToDefinition(
          """
              |Contract Test() {
              |
              |  pub fn test() -> () {
              |    while (true) {
              |      while (true) {
              |        let b = inner2
              |        while (true) {
              |          while (true) {
              |            let copyVar = variabl@@e
              |          }
              |        }
              |      }
              |      let >>variable<< = 1
              |      let variable = 1
              |    }
              |  }
              |
              |}
              |""".stripMargin
        )
      }

      "define in while loop and accessed in for loop" in {
        goToDefinition(
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      while (true) {
            |        let b = inner2
            |        while (true) {
            |          for (let mut index = 0; index < 2; index = index + 1) {
            |            let copyVar = variabl@@e
            |          }
            |        }
            |      }
            |      let >>variable<< = 1
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
        goToDefinition(
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while(true) {
            |      let variable = 1
            |    }
            |    while (true) {
            |      let copyVar = variabl@@e
            |    }
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "defined after usage" in {
        goToDefinition(
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      let copyVar = variabl@@e
            |    }
            |    for (let mut index = 0; index < 2; index = index + 1) {
            |      let variable = 1
            |    }
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "defined after usage but in an inner scope" in {
        goToDefinition(
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      let copyVar = variabl@@e
            |      for (let mut index = 0; index < 2; index = index + 1) {
            |        let variable = 1
            |      }
            |    }
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "defined in a for loop" in {
        goToDefinition(
          """
            |Contract Test() {
            |
            |  pub fn test() -> () {
            |    while (true) {
            |      let copyIndex = inde@@x
            |      for (let mut index = 0; index < 2; index = index + 1) {
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
