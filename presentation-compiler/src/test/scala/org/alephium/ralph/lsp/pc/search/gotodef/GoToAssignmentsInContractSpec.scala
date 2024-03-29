package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToAssignmentsInContractSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "assigned variable does not exist" in {
      goTo(
        """
          |Contract GoToAssignment() {
          |
          |  pub fn function() -> () {
          |    counte@@r = counter + 1
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "assigned variables exist" when {
      "locally in the function" in {
        goTo(
          """
            |Contract GoToAssignment() {
            |
            |  pub fn function() -> () {
            |    >>let counter = 0
            |    <<counte@@r = counter + 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "as function argument" in {
        goTo(
          """
            |Contract GoToAssignment() {
            |
            |  pub fn function(>>mut counter: U256<<) -> () {
            |    counte@@r = counter + 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "as template argument" in {
        goTo(
          """
            |Contract GoToAssignment(>>mut counter: U256<<) {
            |
            |  pub fn function(mut bool: Bool) -> () {
            |    counte@@r = counter + 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "at multiple locations" in {
        goTo(
          """
            |Contract GoToAssignment(>>mut counter: U256<<) {
            |
            |  pub fn function(>>mut counter: U256<<) -> () {
            |    >>let mut counter = 0
            |    <<counte@@r = counter + 1
            |    for (>>let mut counter = 0<<; counter <= 4; counter = counter + 1) {
            |      counter = counter + 1
            |    }
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }
}
