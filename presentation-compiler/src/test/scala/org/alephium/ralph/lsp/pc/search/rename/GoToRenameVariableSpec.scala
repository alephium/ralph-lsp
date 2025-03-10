// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class GoToRenameVariableSpec extends AnyWordSpec with Matchers {

  "rename self" when {
    "there are no references" in {
      goToRename(
        """
          |Contract >>Tes@@t<<(variable: Bool) {
          |  fn test() -> () { }
          |}
          |""".stripMargin
      )
    }
  }

  "rename all occurrences" when {
    "contract type is renamed" in {
      goToRenameForAll(">>Parent<<".r, ">>Pare@@nt<<")(
        """
          |Abstract Contract >>Paren@@t<<(variable: Bool) extends Parent2(variable) { }
          |
          |Contract Child(variable: Bool)
          |               extends >>Parent<<(variable) {
          |
          |  fn child(param: >>Parent<<) -> () {
          |
          |    while(true) {
          |      let _ = >>Parent<<.encodeFields!()
          |    }
          |
          |  }
          |
          |}
          |""".stripMargin
      )
    }

    "duplicate contracts exist" in {
      goToRenameForAll(">>Parent<<".r, ">>Pare@@nt<<")(
        """
          |Abstract Contract >>Paren@@t<<(variable: Bool) { }
          |
          |Abstract Contract >>Parent<<(variable: Bool) { }
          |""".stripMargin
      )
    }
  }

  "rename variables in scope" when {
    "renamed under a while loop (first)" in {
      goToRenameForAll(">>variable<<".r, ">>variab@@le<<")(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    while(true) {
          |      let >>variab@@le<< = true
          |      let copy = >>variable<<
          |      let copy = >>variable<<
          |    }
          |
          |    while(true) {
          |      let variable = true
          |      let copy = variable
          |      let copy = variable
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }

    "renamed under a while loop (second)" in {
      goToRenameForAll(">>variable<<".r, ">>variab@@le<<")(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    while(true) {
          |      let variable = true
          |      let copy = variable
          |      let copy = variable
          |    }
          |
          |    while(true) {
          |      let >>variab@@le<< = true
          |      let copy = >>variable<<
          |      let copy = >>variable<<
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }

    "renamed" in {
      goToRenameForAll(">>variable<<".r, ">>variab@@le<<")(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    let >>variab@@le<< = true
          |
          |    while(true) {
          |      let >>variable<< = true
          |      let copy = >>variable<<
          |      let copy = >>variable<<
          |    }
          |
          |    while(true) {
          |      let >>variable<< = true
          |      let copy = >>variable<<
          |      let copy = >>variable<<
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
