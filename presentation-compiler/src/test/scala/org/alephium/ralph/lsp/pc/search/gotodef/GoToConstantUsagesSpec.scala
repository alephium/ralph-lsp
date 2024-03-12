package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToConstantUsagesSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "constant has not usage" in {
      goTo(
        """
          |Contract GoToConstant() {
          |
          |  const MyCons@@tant = 0
          |
          |  pub fn function() -> () {
          |
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "constant has multiple usages" when {
      def doTest(contractName: String): Unit =
        goTo(
          s"""
             |Contract $contractName() {
             |
             |  const MyCons@@tant = 0
             |  const MyConstant_B = 1
             |
             |  pub fn function() -> () {
             |    let my_constant = >>MyConstant<<
             |    let my_constant2 = >>MyConstant<<
             |    let my_constant3 = MyConstant_B
             |    for (let mut index = 0; index <= 4; index = index + 1) {
             |      let my_constant4 = >>MyConstant<<
             |      let my_constant5 = MyConstant_B
             |    }
             |  }
             |}
             |""".stripMargin
        )


      "constant and contract have the same ID" in {
        // the constant name is also "MyConstant"
        doTest(contractName = "MyConstant")
      }

      "constant and contract have unique IDs" in {
        doTest(contractName = "MyContract")
      }

    }
  }
}
