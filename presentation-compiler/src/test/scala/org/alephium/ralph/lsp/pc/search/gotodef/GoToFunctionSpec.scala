package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToFunctionSpec extends AnyWordSpec with Matchers {

  "return in empty" when {
    "function does not exist" in {
      goTo(
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

  "go to the function" when {
    "function exists" in {
      goTo(
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
      goTo(
        """
          |Contract MyContract(interface: MyInterface) {
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
        goTo(
          """
            |Abstract Contract Test() {
            |
            |  >>fn function() -> ()<<
            |
            |  >>fn function(address: Address) -> ()<<
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
