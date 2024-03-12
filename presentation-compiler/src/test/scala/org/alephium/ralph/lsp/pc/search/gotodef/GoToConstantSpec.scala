package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToConstantSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "constant does not exist" in {
      goTo(
        """
          |Contract GoToConstant() {
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "constant exists" in {
      goTo(
        """
          |Contract GoToConstant() {
          |
          |  >>const MyConstant = 0<<
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "duplicate constants exists" in {
      goTo(
        """
          |Contract GoToConstant() {
          |
          |  >>const MyConstant = 0<<
          |  >>const MyConstant = 1<<
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "constant and the Contract have the same name" in {
      goTo(
        """
          |Contract MyConstant() {
          |
          |  >>const MyConstant = 0<<
          |  >>const MyConstant = 1<<
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }
  }
}
