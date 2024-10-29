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

class GoToRenameTemplateArgumentSpec extends AnyWordSpec with Matchers {

  "rename self" when {
    "there are no references" in {
      goToRename {
        """
          |Contract Test(>>varia@@ble<<: Bool) {
          |  fn test() -> () { }
          |}
          |""".stripMargin
      }
    }
  }

  "rename all occurrences" when {
    "arguments are not accessed" in {
      goToRenameForAll(">>variable<<".r, ">>varia@@ble<<") {
        """
          |Abstract Contract GreatGrandParent(>>varia@@ble<<: Bool) { }
          |
          |Abstract Contract GrandParent(>>variable<<: Bool)
          |                              extends GreatGrandParent(>>variable<<) { }
          |
          |Abstract Contract Parent(>>variable<<: Bool)
          |                        extends GrandParent(>>variable<<) { }
          |
          |Contract Child(>>variable<<: Bool)
          |               extends Parent(>>variable<<) {
          |  fn child() -> () { }
          |}
          |""".stripMargin
      }
    }

    "argument is accessed" when {
      "by GreatGrandParent" in {
        goToRenameForAll(">>variable<<".r, ">>varia@@ble<<") {
          """
            |Abstract Contract GreatGrandParent(>>varia@@ble<<: Bool) {
            |  fn great_grand_parent() -> () {
            |    let copy = >>variable<<
            |  }
            |}
            |
            |Abstract Contract GrandParent(>>variable<<: Bool)
            |                             extends GreatGrandParent(>>variable<<) { }
            |
            |Abstract Contract Parent(>>variable<<: Bool)
            |                        extends GrandParent(>>variable<<) { }
            |
            |Contract Child(>>variable<<: Bool)
            |               extends Parent(>>variable<<) {
            |  fn child() -> () { }
            |}
            |""".stripMargin
        }
      }

      "by GrandParent" in {
        goToRenameForAll(">>variable<<".r, ">>varia@@ble<<") {
          """
            |Abstract Contract GreatGrandParent(>>varia@@ble<<: Bool) { }
            |
            |Abstract Contract GrandParent(>>variable<<: Bool)
            |                             extends GreatGrandParent(>>variable<<) {
            |  fn grand_parent() -> () {
            |    let copy = >>variable<<
            |  }
            |}
            |
            |Abstract Contract Parent(>>variable<<: Bool)
            |                        extends GrandParent(>>variable<<) { }
            |
            |Contract Child(>>variable<<: Bool)
            |               extends Parent(>>variable<<) {
            |  fn child() -> () { }
            |}
            |""".stripMargin
        }
      }

      "by Child" in {
        goToRenameForAll(">>variable<<".r, ">>varia@@ble<<") {
          """
            |Abstract Contract GreatGrandParent(>>varia@@ble<<: Bool) { }
            |
            |Abstract Contract GrandParent(>>variable<<: Bool)
            |                             extends GreatGrandParent(>>variable<<) { }
            |
            |Abstract Contract Parent(>>variable<<: Bool)
            |                        extends GrandParent(>>variable<<) { }
            |
            |Contract Child(>>variable<<: Bool)
            |               extends Parent(>>variable<<) {
            |  fn child() -> () {
            |    let copy = >>variable<<
            |  }
            |}
            |""".stripMargin
        }
      }

      "by all contracts" in {
        goToRenameForAll(">>variable<<".r, ">>varia@@ble<<") {
          """
            |Abstract Contract GreatGrandParent(>>varia@@ble<<: Bool) {
            |  fn great_grand_parent() -> () {
            |    let copy = >>variable<<
            |  }
            |}
            |
            |Abstract Contract GrandParent(>>variable<<: Bool)
            |                             extends GreatGrandParent(>>variable<<) {
            |  fn grand_parent() -> () {
            |    let copy = >>variable<<
            |  }
            |}
            |
            |Abstract Contract Parent(>>variable<<: Bool)
            |                        extends GrandParent(>>variable<<) {
            |  fn parent() -> () {
            |    let copy = >>variable<<
            |  }
            |}
            |
            |Contract Child(>>variable<<: Bool)
            |               extends Parent(>>variable<<) {
            |  fn child() -> () {
            |    while(true) {
            |      let copy = >>variable<<
            |    }
            |    while(true) {
            |      let copy = >>variable<<
            |    }
            |  }
            |}
            |""".stripMargin
        }
      }
    }
  }

}
