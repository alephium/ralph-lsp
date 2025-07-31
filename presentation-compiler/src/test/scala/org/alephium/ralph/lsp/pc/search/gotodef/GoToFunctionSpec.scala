// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToFunctionSpec extends AnyWordSpec with Matchers {

  "return in empty" when {
    "function does not exist" in {
      goToDefinition() {
        """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    let go_to_function = func@@tion_b()
          |    let result = blah.function()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "return self" when {
    "the function itself is selected" in {
      goToDefinition() {
        """
          |Abstract Contract Action() {
          |  fn >>funct@@ion<<() -> Bool
          |}
          |
          |""".stripMargin
      }
    }

    "duplicate functions exist" when {
      "second duplicate is selected" should {
        "still select only itself" in {
          goToDefinition() {
            """
              |Abstract Contract Action() {
              |  fn function() -> Bool
              |
              |  fn >>funct@@ion<<() -> Bool
              |}
              |
              |""".stripMargin
          }
        }
      }
    }
  }

  "go to the function" when {
    "function exists" in {
      goToDefinition() {
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
      }
    }

    "function and arguments have same names" when {
      "strict-parseable" in {
        goToDefinition() {
          """
            |Abstract Contract Parent2() {
            |  pub fn >>function_b<<(boolean: Bool) -> () { }
            |}
            |
            |Abstract Contract Parent1() {
            |  pub fn >>function_b<<(boolean: Bool) -> () { }
            |}
            |
            |Contract MyContract(function_b: Bool) extends Parent1(), Parent2() {
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
        }
      }

      "soft-parseable" in {
        goToDefinition() {
          """
            |Abstract Contract Parent2() {
            |  pub fn >>function_b<<(boolean: Bool) -> () { }
            |}
            |
            |Abstract Contract Parent1() {
            |  pub fn >>function_b<<(boolean: Bool) -> () { }
            |}
            |
            |Contract MyContract(function_b: Bool) extends Parent1(), Parent2() {
            |
            |  // function_b is also an input parameter, but it should still go to the target function.
            |  pub fn function_a(function_b: Bool) -> () {
            |
            |    // Before Usage: nested function, but it is still within scope
            |    fn >>function_b<<(function_b: Bool) -> () {
            |      // different scope
            |      fn function_b(function_b: Bool) -> () { }
            |    }
            |
            |    let go_to_function = func@@tion_b()
            |
            |    // After Usage: nested function, but it is still within scope
            |    fn >>function_b<<(function_b: Bool) -> () {
            |      // different scope
            |      fn function_b(function_b: Bool) -> () { }
            |    }
            |  }
            |
            |  pub fn >>function_b<<(boolean: Bool) -> () {
            |
            |  }
            |}
            |""".stripMargin
        }
      }
    }

    "function is an interface function" should {
      "highlight the entire function signature" in {
        goToDefinition() {
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
            |    functio@@n()
            |  }
            |
            |}
            |""".stripMargin
        }
      }
    }
  }

  "include abstract function in the search result" in {
    goToDefinitionForAll(">>function<<".r, ">>functi@@on<<", testGoToDefSetting.copy(includeAbstractFuncDef = true)) {
      """
        |Abstract Contract GrandParent() {
        |  fn >>function<<() -> ()
        |}
        |
        |Abstract Contract Parent() extends GrandParent() {
        |  fn >>function<<() -> ()
        |}
        |
        |Contract Child() extends Parent() {
        |  fn >>functi@@on<<() -> () {
        |
        |  }
        |}
        |""".stripMargin
    }
  }

  "detect call syntax" should {
    "return the function definition" when {
      "function-call is selected" when {
        "the function exists" in {
          goToDefinition() {
            """
              |{
              |  let function = 1
              |  fn >>function<<() -> () {}
              |  let call = functio@@n()
              |}
              |""".stripMargin
          }
        }

        "the function is called within another function" in {
          goToDefinition() {
            """
              |{
              |  let function = 1
              |
              |  fn >>function<<() {}
              |
              |  fn test() -> () {
              |    let call = functio@@n()
              |  }
              |}
              |""".stripMargin
          }
        }

        "multiple functions and variables exist" in {
          goToDefinition() {
            """
              |{
              |  let function =
              |
              |  fn >>function<<() -> () {}
              |
              |  fn test() -> ( {
              |    let function
              |    let call = functio@@n()
              |    let function = 3
              |  }
              |
              |  fn >>function<<() -> () {}
              |
              |  fn >>function<<() -> ()
              |}
              |""".stripMargin
          }
        }

        "recursive function call" in {
          goToDefinition() {
            """
              |fn >>function<<() -> () {
              |  let call = functio@@n()
              |  let function = 1
              |""".stripMargin
          }
        }

        "function is declared within inheritance" in {
          goToDefinition() {
            """
              |Abstract Contract GrandParent(function) {
              |  let function = 1
              |
              |  fn >>function<<() -> A {
              |    let function = 1
              |  }
              |
              |  let function
              |
              |  fn >>function<<() -> E
              |}
              |
              |Contract Parent(function) extends GrandParent() {
              |
              |  fn >>function<<() -> B {
              |    let call = functio@@n()
              |  }
              |
              |  fn >>function<<() -> C {}
              |
              |  fn >>function<<() ->
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
      "the variable does not exist" in {
        goToDefinition() {
          """
            |{
            |  let var = 1
            |  fn >>variable<<() -> () {}
            |  let copy = variab@@le
            |}
            |""".stripMargin
        }
      }

      "the variable is accessed within a function" in {
        goToDefinition() {
          """
            |{
            |  let var = 1
            |
            |  fn >>variable<<() {}
            |
            |  fn test() -> () {
            |    let copy = variab@@le
            |  }
            |}
            |""".stripMargin
        }
      }

      "multiple functions named `variable` and variables exist" in {
        goToDefinition() {
          """
            |{
            |  let var =
            |
            |  fn >>variable<<() -> () {}
            |
            |  fn test() -> ( {
            |    let var
            |    let copy = variab@@le
            |    let var = 3
            |  }
            |
            |  fn >>variable<<() -> () {}
            |
            |  fn >>variable<<() -> ()
            |}
            |""".stripMargin
        }
      }

      "recursive function named `variable`" in {
        goToDefinition() {
          """
            |fn >>variable<<() -> () {
            |  let copy = variab@@le
            |  let var = 1
            |""".stripMargin
        }
      }

      "function named `variable` is declared within inheritance" in {
        goToDefinition() {
          """
            |Abstract Contract GrandParent() {
            |  let var = 1
            |
            |  fn >>variable<<() -> A {
            |    let var = 1
            |  }
            |
            |  let var
            |
            |  fn >>variable<<() -> E
            |}
            |
            |Contract Parent() extends GrandParent() {
            |
            |  fn >>variable<<() -> B {
            |    let copy = variab@@le
            |  }
            |
            |  fn >>variable<<() -> C {}
            |
            |  fn >>variable<<() ->
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
