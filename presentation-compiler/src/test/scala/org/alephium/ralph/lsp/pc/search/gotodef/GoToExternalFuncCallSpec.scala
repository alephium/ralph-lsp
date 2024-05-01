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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToExternalFuncCallSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "external function does not exist" in {
      goTo(
        """
          |Contract Main(action: Action) {
          |  pub fn main() -> () {
          |    let result = action.act@@()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "external abstract function exists" should {
      "go from template parameter" in {
        goTo(
          """
            |Abstract Contract Action() {
            |  >>fn function() -> Bool<<
            |}
            |
            |Contract Main(action: Action) {
            |  pub fn main() -> () {
            |    let result = action.function@@()
            |  }
            |}
            |""".stripMargin
        )
      }

      "go from function parameter" in {
        goTo(
          """
            |Abstract Contract Action() {
            |  >>fn function() -> Bool<<
            |}
            |
            |Contract Main() {
            |  pub fn main(action: Action) -> () {
            |    let result = action.function@@()
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "external function exists" should {
      "go from template parameter" in {
        goTo(
          """
            |Contract Action() {
            |  fn >>function<<() -> Bool {
            |    return true
            |  }
            |}
            |
            |Contract Main(action: Action) {
            |  pub fn main() -> () {
            |    let result = action.function@@()
            |  }
            |}
            |""".stripMargin
        )
      }

      "go from function parameter" in {
        goTo(
          """
            |Contract Action() {
            |  fn >>function<<() -> Bool {
            |    return true
            |  }
            |}
            |
            |Contract Main() {
            |  pub fn main(action: Action) -> () {
            |    let result = action.function@@()
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "external function exists in nested hierarchy" in {
      goTo(
        """
            |Interface Parent2 {
            |  fn not_used2() -> ()
            |
            |  >>fn function() -> ()<<
            |}
            |
            |Abstract Contract Parent1() implements Parent2 {
            |  fn not_used1() -> ()
            |
            |  fn >>function<<() -> () {
            |
            |  }
            |}
            |
            |Abstract Contract Action0() extends Parent1() {
            |  fn not_used0() -> ()
            |}
            |
            |Contract Main(action: Action0) {
            |  pub fn main() -> () {
            |    let result = action.function@@()
            |  }
            |}
            |""".stripMargin
      )
    }
  }

}
