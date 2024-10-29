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

class GoToRenameEnumTypeSpec extends AnyWordSpec with Matchers {

  "rename self" when {
    "there are no references" in {
      goToRename(
        """
          |Abstract Contract Test() {
          |  enum >>MyEn@@um<< {
          |    ONE = 1
          |    TWO = 2
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "rename all occurrences" when {
    "enum type is renamed" when {
      "local enum" in {
        goToRenameForAll(">>MyEnum<<".r, ">>MyEn@@um<<")(
          """
            |Abstract Contract Parent() {
            |  enum >>MyEn@@um<< {
            |    ONE = 1
            |    TWO = 2
            |  }
            |
            |  fn parent(contract: MyEnum) -> () {
            |    let _ = >>MyEnum<<.Blah
            |    let _ = >>MyEnum<<.TWO
            |  }
            |}
            |
            |Contract Child() extends Parent() {
            |
            |  fn child(contract: MyEnum) -> () {
            |    while(true) {
            |      let _ = >>MyEnum<<.Blah
            |      let _ = >>MyEnum<<.TWO
            |    }
            |  }
            |
            |}
            |""".stripMargin
        )
      }

      "public enum" in {
        goToRenameForAll(">>TestEnum<<".r, ">>TestEn@@um<<")(
          """
            |enum >>TestEn@@um<< {
            |  ONE = 1
            |  TWO = 2
            |}
            |
            |Abstract Contract Parent() {
            |  fn parent(contract: TestEnum) -> () {
            |    let _ = >>TestEnum<<.Blah
            |    let _ = >>TestEnum<<.TWO
            |  }
            |}
            |
            |Contract Child() extends Parent() {
            |  fn child(contract: TestEnum) -> () {
            |    while(true) {
            |      let _ = >>TestEnum<<.Blah
            |      let _ = >>TestEnum<<.TWO
            |    }
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "duplicate contracts exist" in {
      goToRenameForAll(">>TestEnum<<".r, ">>TestEn@@um<<")(
        """
          |enum >>TestEn@@um<< {
          |  ONE = 1
          |  TWO = 2
          |}
          |
          |Abstract Contract Parent() {
          |
          |  enum >>TestEnum<< {
          |    THREE = 3
          |    FOUR = 4
          |  }
          |
          |  fn parent(contract: TestEnum) -> () {
          |    let a = >>TestEnum<<.Blah
          |    let b = >>TestEnum<<.TWO
          |    let c = >>TestEnum<<.FOUR
          |  }
          |}
          |
          |Contract Child() extends Parent() {
          |  fn child(contract: TestEnum) -> () {
          |    while(true) {
          |      let a = >>TestEnum<<.Blah
          |      let b = >>TestEnum<<.TWO
          |      let c = >>TestEnum<<.FOUR
          |    }
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
