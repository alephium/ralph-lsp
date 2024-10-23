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

class GoToLocalVariableSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable does not exist" in {
      goToDefinition()(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    // varB does not exists
          |    let varA = v@@arB
          |  }
          |
          |}
          |""".stripMargin
      )
    }
  }

  "return self" when {
    "variable itself is selected" in {
      goToDefinition()(
        """
          |Contract Test() {
          |
          |  pub fn function() -> () {
          |    let >>var@@A<< = 123
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "duplicate variables exists" when {
      "first var is selected" in {
        goToDefinition()(
          """
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let >>var@@A<< = 123
            |    let varA = 123
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "second var is selected" in {
        goToDefinition()(
          """
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let varA = 123
            |    let >>var@@A<< = 123
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }
  }

  "return non-empty" when {
    "single local variable exists" in {
      goToDefinition()(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    let >>varA<< = 123
          |    let varB = var@@A
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "multiple local variables exists" in {
      goToDefinition()(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    let >>varA<< = 123
          |    let varB = var@@A
          |    let varA = ABC
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "local variable and arguments have the same name" in {
      goToDefinition()(
        """
          |Contract GoToTest(>>varA<<: Bool) {
          |
          |  pub fn function(>>varA<<: Bool) -> () {
          |    let >>varA<< = 123
          |    let varB = var@@A
          |    for (let mut varA = 0; varA <= 4; varA = varA + 1) {
          |       function(true)
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }

    "variable is in an ApproveAsset expression" in {
      goToDefinition()(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    let >>varA<< = 123
          |    obj.fun{builtIn!() -> ALPH: varA@@}(somethingElse)
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "variable is a tuple" when {
      "first tuple is queried" in {
        goToDefinition()(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let (>>first<<, second) = getTuple()
            |
            |    function(
            |      firs@@t,
            |      second
            |    )
            |  }
            |}
            |""".stripMargin
        )
      }

      "second tuple is queried" in {
        goToDefinition()(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let (first, >>second<<) = getTuple()
            |
            |    function(
            |      first,
            |      secon@@d
            |    )
            |  }
            |}
            |""".stripMargin
        )
      }

      "there are duplicate tuples" in {
        goToDefinition()(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let (first, >>second<<) = getTuple()
            |    let (first, >>second<<, third) = getTuple()
            |    let (first, >>second<<, third, fourth) = getTuple()
            |
            |    function(
            |      first,
            |      secon@@d
            |    )
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }

}
