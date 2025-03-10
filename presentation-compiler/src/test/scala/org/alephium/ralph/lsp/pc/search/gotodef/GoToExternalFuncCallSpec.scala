// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToExternalFuncCallSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "external function does not exist" in {
      goToDefinitionStrict()(
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
        goToDefinitionStrict()(
          """
            |Abstract Contract Action() {
            |  fn >>function<<() -> Bool
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
        goToDefinitionStrict()(
          """
            |Abstract Contract Action() {
            |  fn >>function<<() -> Bool
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
        goToDefinitionStrict()(
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
        goToDefinitionStrict()(
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
      goToDefinitionStrict()(
        """
            |Interface Parent2 {
            |  fn not_used2() -> ()
            |
            |  fn >>function<<() -> ()
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
