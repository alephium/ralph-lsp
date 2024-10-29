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

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToRenameFunctionSpec extends AnyWordSpec with Matchers {

  "not rename" when {
    "overloaded functions do not have an abstract function" in {
      goToRenameForAll(">>function<<".r, ">>functi@@on<<") {
        """
          |Abstract Contract Parent() {
          |  fn function() -> () {
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

  "rename successfully" when {
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

}
