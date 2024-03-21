package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToLocalVariableUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable is not used" in {
      goTo(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    // varA is not used
          |    let va@@rA = varB
          |  }
          |
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "a single usage exists" in {
      goTo(
        """
          |Contract GoToTest() {
          |
          |  pub fn function() -> () {
          |    let va@@rA = 123
          |    let varB = >>varA<<
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
          |    let varA = >>varB<<
          |    let varB@@ = varA
          |    let varC = >>varB<<
          |    obj.fun{builtIn!() -> ALPH: >>varB<<}(somethingElse)
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "local variable and arguments have the same name" in {
      goTo(
        """
          |Contract GoToTest(varA: Bool) {
          |
          |  pub fn function(varA: Bool) -> () {
          |    let varA = 123
          |    let varB@@ = varA
          |    for (let mut index = >>varB<<;
          |         index <= 4;
          |         index = >>varB<< + 1) {
          |       function(>>varB<<)
          |       obj.fun{builtIn!() -> ALPH: >>varB<<}(somethingElse)
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
