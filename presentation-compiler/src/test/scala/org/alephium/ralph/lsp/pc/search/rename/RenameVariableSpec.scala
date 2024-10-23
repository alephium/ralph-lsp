// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.search.rename

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class RenameVariableSpec extends AnyWordSpec with Matchers {

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
