// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HoverFunctionSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "function does not exist" in {
      hover() {
        """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    let foo = func@@tion()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "return self" when {
    "the blockless function itself is selected" in {
      hover() {
        """
          |Abstract Contract Action() {
          |  >>fn funct@@ion() -> Bool<<
          |}
          |
          |""".stripMargin
      }
    }

    "the function itself is selected" in {
      hover() {
        """
          |Abstract Contract Action() {
          |  >>fn funct@@ion() -> Bool<<{
          |    true
          |  }
          |}
          |
          |""".stripMargin
      }
    }

    "duplicate functions exist" when {
      "second duplicate is selected" should {
        "still select only itself" in {
          hover() {
            """
              |Abstract Contract Action() {
              |  fn function() -> Bool
              |
              |  >>fn funct@@ion() -> Bool<<
              |}
              |
              |""".stripMargin
          }
        }
      }
    }
  }

  "hover the function" when {
    "function exists" in {
      hover() {
        """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    let go_to_function = func@@tion_b()
          |    let result = blah.function()
          |  }
          |
          |  >>pub fn function_b(boolean: Bool) -> ()<< {
          |
          |  }
          |}
          |""".stripMargin
      }
    }

    "function exits in a parent" in {
      hover() {
        """
            |Abstract Contract Parent() {
            |  >>pub fn function(boolean: Bool) -> ()<< { }
            |}
            |
            |Contract MyContract() extends Parent() {
            |
            |  pub fn foo(boolean: Bool) -> () {
            |    func@@tion(boolean)
            |  }
            |}
            |""".stripMargin
      }
    }

    "function is an interface function" should {
      "aggregate multiple hover function information" in {

        /**
         *  This happen due to the soft parser, a compiler error is raised
         *  but still we can propose every match as hover information.
         */
        hover() {
          """
            |Abstract Contract Test() {
            |
            |  >>fn function() -> ()<<
            |
            |  >>fn function(address: Address) -> ()<<
            |
            |  >>fn function() -> ()<< {
            |     assert!()
            |  }
            |
            |  fn main() -> () {
            |    func@@tion()
            |  }
            |}
            |""".stripMargin
        }
      }
    }

    "function has documentation" in {
      hover() {
        """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    let go_to_function = func@@tion_b()
          |    let result = blah.function()
          |  }
          |
          |  >>// This is a function that does something.
          |  // And it does it well.
          |
          |  pub fn function_b(boolean: Bool) -> ()<< {
          |
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "detect call syntax" should {
    "return the hover info" when {
      "function-call is selected" when {
        "the function exists" in {
          hover() {
            """
              |{
              |  let function = 1
              |  >>fn function() -> ()<< {}
              |  let call = functio@@n()
              |}
              |""".stripMargin
          }
        }

        "the function is called within another function" in {
          hover() {
            """
              |{
              |  let function = 1
              |
              |  >>fn function() <<{}
              |
              |  fn test() -> () {
              |    let call = functio@@n()
              |  }
              |}
              |""".stripMargin
          }
        }

        "multiple functions and variables exist" in {
          hover() {
            """
              |{
              |  let function =
              |
              |  >>fn function() -> ()<< {}
              |
              |  fn test() -> ( {
              |    let function
              |    let call = functio@@n()
              |    let function = 3
              |  }
              |
              |  >>fn function() -> ()<< {}
              |
              |  >>fn function() -> ()<<
              |}
              |""".stripMargin
          }
        }

        "recursive function call" in {
          hover() {
            """
              |>>fn function() -> ()<< {
              |  let call = functio@@n()
              |  let function = 1
              |""".stripMargin
          }
        }

        "function is declared within inheritance" in {
          hover() {
            """
              |Abstract Contract GrandParent(function) {
              |  let function = 1
              |
              |  >>fn function() -> A<< {
              |    let function = 1
              |  }
              |
              |  let function
              |
              |  >>fn function() -> E<<
              |}
              |
              |Contract Parent(function) extends GrandParent() {
              |
              |  >>fn function() -> B<< {
              |    let call = functio@@n()
              |  }
              |
              |  >>fn function() -> C<< {}
              |}
              |
              |Contract Child(function) extends Parent(function) {
              |  // This is available to the `Parent`
              |  let function = 3
              |
              |  fn function() -> D { }
              |}
              |""".stripMargin
          }
        }
      }
    }

    "variable-reference is selected" when {
      "the variable exist" in {
        hover() {
          """
            |{
            |  let var = 1
            |  >>fn variable() -> ()<< {}
            |  let copy = variab@@le
            |}
            |""".stripMargin
        }
      }

      "the variable is accessed within a function" in {
        hover() {
          """
            |{
            |  let var = 1
            |
            |  >>fn variable() -> ()<< {}
            |
            |  fn test() -> () {
            |    let copy = variab@@le
            |  }
            |}
            |""".stripMargin
        }
      }

      "multiple functions named `variable` and variables exist" in {
        hover() {
          """
            |{
            |  let var =
            |
            |  >>fn variable() -> ()<< {}
            |
            |  fn test() -> ( {
            |    let var
            |    let copy = variab@@le
            |    let var = 3
            |  }
            |
            |  >>fn variable() -> ()<< {}
            |
            |  >>fn variable() -> ()<<
            |}
            |""".stripMargin
        }
      }

      "recursive function named `variable`" in {
        hover() {
          """
            |>>fn variable() -> ()<< {
            |  let copy = variab@@le
            |  let var = 1
            |""".stripMargin
        }
      }

      "function named `variable` is declared within inheritance" in {
        hover() {
          """
            |Abstract Contract GrandParent() {
            |  let var = 1
            |
            |  >>fn variable() -> A<< {
            |    let var = 1
            |  }
            |
            |  let var
            |
            |  >>fn variable() -> E<<
            |}
            |
            |Contract Parent() extends GrandParent() {
            |
            |  >>fn variable() -> B<< {
            |    let copy = variab@@le
            |  }
            |
            |  >>fn variable() -> C<< {}
            |}
            |
            |Contract Child(variable) extends Parent(variable) {
            |  let variable = 3
            |
            |  fn variable() -> D { }
            |}
            |""".stripMargin
        }
      }
    }
  }

}
