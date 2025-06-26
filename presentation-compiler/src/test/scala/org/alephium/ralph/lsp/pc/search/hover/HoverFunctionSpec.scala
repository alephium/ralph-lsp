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
      }()
    }
  }

  "return self" when {
    "the blockless function itself is selected" in {
      hover()(code = """
          |Abstract Contract Action() {
          |  fn funct@@ion() -> Bool
          |}
          |
          |""".stripMargin)(
        expected = "fn function() -> Bool"
      )
    }

    "the function itself is selected" in {
      hover()(code = """
          |Abstract Contract Action() {
          |  fn funct@@ion() -> Bool {
          |    true
          |  }
          |}
          |
          |""".stripMargin)(
        expected = "fn function() -> Bool"
      )
    }

    "duplicate functions exist" when {
      "second duplicate is selected" should {
        "still select only itself" in {
          hover()(code = """
              |Abstract Contract Action() {
              |  fn function() -> Bool
              |
              |  fn funct@@ion() -> Bool
              |}
              |
              |""".stripMargin)(
            expected = "fn function() -> Bool"
          )
        }
      }
    }
  }

  "hover the function" when {
    "function exists" in {
      hover()(code = """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    let foo = func@@tion_b()
          |  }
          |
          |  pub fn function_b(boolean: Bool) -> () {
          |
          |  }
          |}
          |""".stripMargin)(
        expected = "pub fn function_b(boolean: Bool) -> ()"
      )
    }

    // FIXME: Currently we are not reformatting
    "function is not formatted" in {
      hover()(code = """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean : Bool) -> () {
          |    let foo = func@@tion_b()
          |  }
          |
          |    pub   fn  function_b  (
          | boolean  :  Bool
          |) ->    () {
          |  }
          |}
          |""".stripMargin)(
        expected = """|pub   fn  function_b  (
                      | boolean  :  Bool
                      |) ->    ()""".stripMargin
      )
    }
    "function exits in a parent" in {
      hover()(code = """
            |Abstract Contract Parent() {
            |  pub fn function(boolean: Bool) -> () { }
            |}
            |
            |Contract MyContract() extends Parent() {
            |
            |  pub fn foo(boolean: Bool) -> () {
            |    func@@tion(boolean)
            |  }
            |}
            |""".stripMargin)(
        expected = "pub fn function(boolean: Bool) -> ()"
      )
    }

    "function is an interface function" should {
      "aggregate multiple hover function information" in {

        /**
         *  This happen due to the soft parser, a compiler error is raised
         *  but still we can propose every match as hover information.
         */
        hover()(code = """
            |Abstract Contract Test() {
            |
            |  fn function() -> ()
            |
            |  fn function(address: Address) -> ()
            |
            |  fn function() -> () {
            |     assert!()
            |  }
            |
            |  fn main() -> () {
            |    func@@tion()
            |  }
            |}
            |""".stripMargin)(
          expected = List(
            "fn function() -> ()",
            "fn function(address: Address) -> ()",
            "fn function() -> ()"
          ): _*
        )
      }
    }

    "function has documentation" in {
      hover()(code = """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    let foo = func@@tion_b()
          |    let result = blah.function()
          |  }
          |
          |  // This is a function that does something.
          |  // And it does it well.
          |
          |  pub fn function_b(boolean: Bool) -> () {
          |
          |  }
          |}
          |""".stripMargin)(
        expected = """|// This is a function that does something.
             |  // And it does it well.
             |
             |  pub fn function_b(boolean: Bool) -> ()""".stripMargin
      )
    }

    "function has annotation" in {
      hover()(code = """
          |Contract MyContract(interface: MyInterface) {
          |  pub fn function_a(boolean: Bool) -> () {
          |    func@@tion_b(true)
          |  }
          |
          |  @using(checkExternalCaller = false)
          |  pub fn function_b(boolean: Bool) -> () {
          |
          |  }
          |}
          |""".stripMargin)(
        expected = """ |@using(checkExternalCaller = false)
                       |  pub fn function_b(boolean: Bool) -> ()""".stripMargin
      )
    }
  }

  "detect call syntax" should {
    "return the hover info" when {
      "function-call is selected" when {
        "the function exists" in {
          hover()(code = """
              |{
              |  let function = 1
              |  fn function() -> () {}
              |  let call = functio@@n()
              |}
              |""".stripMargin)(
            expected = "fn function() -> ()"
          )
        }

        "the function is called within another function" in {
          hover()(code = """
              |{
              |  let function = 1
              |
              |  fn function() {}
              |
              |  fn test() -> () {
              |    let call = functio@@n()
              |  }
              |}
              |""".stripMargin)(
            expected = "fn function() "
          )
        }

        "multiple functions and variables exist" in {
          hover()(code = """
              |{
              |  let function =
              |
              |  fn function() -> () {}
              |
              |  fn test() -> ( {
              |    let function
              |    let call = functio@@n()
              |    let function = 3
              |  }
              |
              |  fn function() -> () {}
              |
              |  fn function() -> ()
              |}
              |""".stripMargin)(
            expected = List(
              "fn function() -> ()",
              "fn function() -> ()",
              "fn function() -> ()"
            ): _*
          )
        }

        "recursive function call" in {
          hover()(code = """
              |fn function() -> () {
              |  let call = functio@@n()
              |  let function = 1
              |""".stripMargin)(
            expected = "fn function() -> ()"
          )
        }

        "function is declared within inheritance" in {
          hover()(code = """
              |Abstract Contract GrandParent(function) {
              |  let function = 1
              |
              |  fn function() -> A {
              |    let function = 1
              |  }
              |
              |  let function
              |
              |  fn function() -> E
              |}
              |
              |Contract Parent(function) extends GrandParent() {
              |
              |  fn function() -> B {
              |    let call = functio@@n()
              |  }
              |
              |  fn function() -> C {}
              |}
              |
              |Contract Child(function) extends Parent(function) {
              |  // This is available to the `Parent`
              |  let function = 3
              |
              |  fn function() -> D { }
              |}
              |""".stripMargin)(
            expected = List(
              "fn function() -> A",
              "fn function() -> B",
              "fn function() -> C",
              "fn function() -> E"
            ): _*
          )
        }
      }

      "variable-reference is selected" when {
        "the riable exist" in {
          hover()(code = """
            |{
            |  let var = 1
            |  fn variable() -> () {}
            |  let copy = variab@@le
            |}
            |""".stripMargin)(
            expected = "fn variable() -> ()"
          )
        }
      }

      "the variable is accessed within a function" in {
        hover()(code = """
            |{
            |  let var = 1
            |
            |  fn variable() -> () {}
            |
            |  fn test() -> () {
            |    let copy = variab@@le
            |  }
            |}
            |""".stripMargin)(
          expected = "fn variable() -> ()"
        )
      }

      "multiple functions named `variable` and variables exist" in {
        hover()(code = """
            |{
            |  let var =
            |
            |  fn variable() -> () {}
            |
            |  fn test() -> ( {
            |    let var
            |    let copy = variab@@le
            |    let var = 3
            |  }
            |
            |  fn variable() -> () {}
            |
            |  fn variable() -> ()
            |}
            |""".stripMargin)(
          expected = List(
            "fn variable() -> ()",
            "fn variable() -> ()",
            "fn variable() -> ()"
          ): _*
        )
      }

      "recursive function named `variable`" in {
        hover()(code = """
            |fn variable() -> () {
            |  let copy = variab@@le
            |  let var = 1
            |""".stripMargin)(
          expected = "fn variable() -> ()"
        )
      }

      "function named `variable` is declared within inheritance" in {
        hover()(code = """
            |Abstract Contract GrandParent() {
            |  let var = 1
            |
            |  fn variable() -> A {
            |    let var = 1
            |  }
            |
            |  let var
            |
            |  fn variable() -> E
            |}
            |
            |Contract Parent() extends GrandParent() {
            |
            |  fn variable() -> B {
            |    let copy = variab@@le
            |  }
            |
            |  fn variable() -> C {}
            |}
            |
            |Contract Child(variable) extends Parent(variable) {
            |  let variable = 3
            |
            |  fn variable() -> D { }
            |}
            |""".stripMargin)(
          expected = List(
            "fn variable() -> A",
            "fn variable() -> B",
            "fn variable() -> C",
            "fn variable() -> E"
          ): _*
        )
      }
    }
  }

  "hover function within the workspace" should {
    "function exits in a workspace parent" in {
      hover()(
        code = """
        |Abstract Contract Parent() {
        |  pub fn function(boolean: Bool) -> () { }
        |}
        |""".stripMargin,
        """
        |Contract MyContract() extends Parent() {
        |
        |  pub fn foo(boolean: Bool) -> () {
        |    func@@tion(boolean)
        |  }
        |}
        |""".stripMargin
      )(
        expected = "pub fn function(boolean: Bool) -> ()"
      )
    }

    "function comes from another workspace contract" in {
      hover()(
        code = """
        |Contract Bar() {
        |  pub fn bar(boolean: Bool) -> () { }
        |}
        |""".stripMargin,
        """
        |Contract MyContract() {
        |
        |  pub fn foo(bar: Bar) -> () {
        |    bar.b@@ar(true)
        |  }
        |}
        |""".stripMargin
      )(
        expected = "pub fn bar(boolean: Bool) -> ()"
      )
    }

    "function with same name comes from another workspace contract" in {
      hover()(
        code = """
        |Contract Bar() {
        |  pub fn foo(boolean: Bool) -> () { }
        |}
        |""".stripMargin,
        """
        |Contract MyContract() {
        |
        |  pub fn foo(bar: Bar) -> () {
        |    bar.fo@@o(true)
        |  }
        |}
        |""".stripMargin
      )(
        expected = "pub fn foo(boolean: Bool) -> ()"
      )
    }
  }

  "hover builtin function" should {
    "return the builtin function" in {
      hover()(code = """
              |Contract Test() {
              |  pub fn function() -> () {
              |    ass@@ert!()
              |  }
              |}
              |""".stripMargin)(
        expected =
          // \u0020 escapes a space, we do this to avoid the editor to remove the trailing space
          s"""|// Tests the condition or checks invariants.
              |  // @param condition the condition to be checked
              |  // @param errorCode the error code to throw if the check fails
              |  // @returns\u0020
              |  fn assert!(condition:Bool, errorCode:U256) -> ()""".stripMargin
      )
    }
  }

}
