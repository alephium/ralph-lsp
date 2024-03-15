package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToLocalVariableSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable does not exist" in {
      goTo(
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

  "return non-empty" when {
    "single local variable exists" in {
      goTo(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    >>let varA = 123
          |    <<let varB = var@@A
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "multiple local variables exists" in {
      goTo(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    >>let varA = 123
          |    <<let varB = var@@A
          |    >>let varA = ABC
          |<<  }
          |
          |}
          |""".stripMargin
      )
    }

    "local variable and arguments have the same name" in {
      goTo(
        """
          |Contract GoToTest(>>varA: Bool<<) {
          |
          |  pub fn function(>>varA: Bool<<) -> () {
          |    >>let varA = 123
          |    <<let varB = var@@A
          |    for (>>let mut varA = 0<<; varA <= 4; varA = varA + 1) {
          |       function(true)
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
