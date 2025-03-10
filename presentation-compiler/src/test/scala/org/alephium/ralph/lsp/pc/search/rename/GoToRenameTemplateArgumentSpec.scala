// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
