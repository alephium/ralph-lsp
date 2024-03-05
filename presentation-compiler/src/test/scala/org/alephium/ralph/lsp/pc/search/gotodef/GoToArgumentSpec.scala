package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeSearcher._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToArgumentSpec extends AnyWordSpec with Matchers {

  "return in empty" when {
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

  "go to a single contract argument" when {
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
  }
}
