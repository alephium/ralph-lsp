package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToArgumentSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "argument does not exists" in {
      goTo(
        """
          |Contract GoToField(interface: MyInterface) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // blah argument does not exists
          |    let result = bl@@ah.function()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "initial character is selected" in {
      goTo(
        """
          |Contract GoToField(>>interface: MyInterface<<) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // first character
          |    let result = @@interface.function()
          |  }
          |}
          |""".stripMargin
      )
    }

    "mid character is selected" in {
      goTo(
        """
          |Contract GoToField(>>interface: MyInterface<<) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // mid character
          |    let result = inte@@rface.function()
          |  }
          |}
          |""".stripMargin
      )
    }

    "last character is selected" in {
      goTo(
        """
          |Contract GoToField(>>interface: MyInterface<<) {
          |  pub fn local_function(boolean: Bool) -> () {
          |    // last character
          |    let result = interface@@.function()
          |  }
          |}
          |""".stripMargin
      )
    }

    "function and the argument have the same name" in {
      goTo(
        """
          |Contract MyContract(interface: MyInterface) {
          |
          |  // argument_b is also a function, but it should still go to the argument.
          |  pub fn function_a(>>argument_b: Bool<<) -> () {
          |    let go_to_function = @@argument_b
          |    let result = blah.function()
          |  }
          |
          |  pub fn argument_b(boolean: Bool) -> () {
          |
          |  }
          |}
          |""".stripMargin
      )
    }

    "there are multiple arguments with the same name" in {
      goTo(
        """
          |// the furthest argument
          |Contract GoToField(>>interface: MyInterface<<) {
          |
          |  // the nearest argument
          |  pub fn local_function(>>interface: MyInterface<<) -> () {
          |    let result = interface@@.function()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
