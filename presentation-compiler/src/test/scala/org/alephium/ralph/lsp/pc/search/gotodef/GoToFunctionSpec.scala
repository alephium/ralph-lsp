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

class GoToFunctionSpec extends AnyWordSpec with Matchers {

  "return in empty" when {
    "function does not exist" in {
      goToDefinition(
        """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    let go_to_function = func@@tion_b()
          |    let result = blah.function()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return self" when {
    "the function itself is selected" in {
      goToDefinition(
        """
          |Abstract Contract Action() {
          |  fn >>funct@@ion<<() -> Bool
          |}
          |
          |""".stripMargin
      )
    }

    "duplicate functions exist" when {
      "second duplicate is selected" should {
        "still select only itself" in {
          goToDefinition(
            """
              |Abstract Contract Action() {
              |  fn function() -> Bool
              |
              |  fn >>funct@@ion<<() -> Bool
              |}
              |
              |""".stripMargin
          )
        }
      }
    }
  }

  "go to the function" when {
    "function exists" in {
      goToDefinition(
        """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    let go_to_function = func@@tion_b()
          |    let result = blah.function()
          |  }
          |
          |  pub fn >>function_b<<(boolean: Bool) -> () {
          |
          |  }
          |}
          |""".stripMargin
      )
    }

    "function and argument have same names" in {
      goToDefinition(
        """
          |Abstract Contract Parent2() {
          |
          |  pub fn >>function_b<<(boolean: Bool) -> () { }
          |}
          |
          |Abstract Contract Parent1() {
          |
          |  pub fn >>function_b<<(boolean: Bool) -> () { }
          |}
          |
          |Contract MyContract(interface: MyInterface) extends Parent1(), Parent2() {
          |
          |  // function_b is also an input parameter, but it should still go to the target function.
          |  pub fn function_a(function_b: Bool) -> () {
          |    let go_to_function = func@@tion_b()
          |    let result = blah.function()
          |  }
          |
          |  pub fn >>function_b<<(boolean: Bool) -> () {
          |
          |  }
          |}
          |""".stripMargin
      )
    }

    "function is an interface function" should {
      "highlight the entire function signature" in {
        goToDefinition(
          """
            |Abstract Contract Test() {
            |
            |  fn >>function<<() -> ()
            |
            |  fn >>function<<(address: Address) -> ()
            |
            |  // this function has a body so only the function ID is highlighted.
            |  fn >>function<<() -> () {
            |     assert!()
            |  }
            |
            |  fn main() -> () {
            |    function@@()
            |  }
            |
            |}
            |""".stripMargin
        )

      }
    }
  }

}
