package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToAssignmentsInTxScriptSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "assigned variable does not exist" in {
      goTo(
        """
          |TxScript GoToAssignment() {
          |  counte@@r = counter + 1
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "assigned variables exist" when {
      goTo(
        """
          |TxScript GoToAssignment(>>counter: U256<<) {
          |  >>let mut counter = 0
          |  <<counte@@r = counter + 1
          |  for (>>let mut counter = 0<<; counter <= 4; counter = counter + 1) {
          |    counter = counter + 1
          |  }
          |}
          |""".stripMargin
      )
    }
  }
}
