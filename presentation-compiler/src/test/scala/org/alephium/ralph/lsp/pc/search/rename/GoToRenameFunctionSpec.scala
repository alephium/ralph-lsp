// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToRenameFunctionSpec extends AnyWordSpec with Matchers {

  "overloaded function is not an abstract function" in {
    goToRenameForAll(">>function<<".r, ">>functi@@on<<") {
      """
        |Abstract Contract Parent() {
        |  fn >>function<<() -> () {
        |
        |  }
        |}
        |
        |Contract Child() extends Parent() {
        |  fn >>functi@@on<<() -> () {
        |
        |  }
        |}
        |""".stripMargin
    }
  }

  "abstract function and its function implementation exist" in {
    goToRenameForAll(">>function<<".r, ">>functi@@on<<") {
      """
        |Abstract Contract Parent() {
        |  fn >>function<<() -> ()
        |}
        |
        |Contract Child() extends Parent() {
        |  fn >>functi@@on<<() -> () {
        |
        |  }
        |}
        |""".stripMargin
    }
  }

  "overloaded abstract functions and their overloaded function implementations exist" in {
    goToRenameForAll(">>function<<".r, ">>functi@@on<<") {
      """
        |Abstract Contract GrandParent() {
        |  fn >>function<<() -> ()
        |}
        |
        |Abstract Contract Parent() extends GrandParent() {
        |  fn >>function<<() -> ()
        |}
        |
        |Contract Child() extends Parent() {
        |  fn >>function<<() -> () {
        |
        |  }
        |}
        |
        |Contract Child() extends Parent() {
        |  fn >>functi@@on<<() -> () {
        |
        |  }
        |}
        |""".stripMargin
    }
  }

}
