// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToExternalFuncCallSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "external function does not exist" in {
      goToDefinition()(
        """
          |Contract Main(action: Action) {
          |  pub fn main() -> () {
          |    let result = action.ac@@t()
          |  }
          |}
          |""".stripMargin
      )
    }

    "external value does not exist" in {
      goToDefinition()(
        """
          |Contract Main(action: Action) {
          |  pub fn main() -> () {
          |    let result = action.ac@@t
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "external abstract function exists" should {
      "go from template parameter" when {
        "function call has closing paren" in {
          goToDefinition()(
            """
              |Abstract Contract Action() {
              |  fn >>function<<() -> Bool
              |}
              |
              |Contract Main(action: Action) {
              |  pub fn main() -> () {
              |    let result = action.functio@@n()
              |  }
              |}
              |""".stripMargin
          )
        }

        "function does not have closing parens" in {
          goToDefinitionSoft()(
            """
              |Abstract Contract Action() {
              |  fn >>function<<() -> Bool
              |}
              |
              |Contract Main(action: Action) {
              |  pub fn main() -> () {
              |    let result = action.functio@@n
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "go from function parameter" when {
        "function call has closing parens" in {
          goToDefinition()(
            """
              |Abstract Contract Action() {
              |  fn >>function<<() -> Bool
              |}
              |
              |Contract Main() {
              |  pub fn main(action: Action) -> () {
              |    let result = action.functio@@n()
              |  }
              |}
              |""".stripMargin
          )
        }

        "function call does not have closing parens" in {
          goToDefinitionSoft()(
            """
              |Abstract Contract Action() {
              |  fn >>function<<() -> Bool
              |}
              |
              |Contract Main() {
              |  pub fn main(action: Action) -> () {
              |    let result = action.functio@@n
              |  }
              |}
              |""".stripMargin
          )
        }
      }
    }

    "external function exists" should {
      "go from template parameter" when {
        "function call has closing parens" in {
          goToDefinition()(
            """
              |Contract Action() {
              |  fn >>function<<() -> Bool {
              |    return true
              |  }
              |}
              |
              |Contract Main(action: Action) {
              |  pub fn main() -> () {
              |    let result = action.functio@@n()
              |  }
              |}
              |""".stripMargin
          )
        }

        "function call does not have closing parens" in {
          goToDefinitionSoft()(
            """
              |Contract Action() {
              |  fn >>function<<() -> Bool {
              |    return true
              |  }
              |}
              |
              |Contract Main(action: Action) {
              |  pub fn main() -> () {
              |    let result = action.functio@@n
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "go from function parameter" when {
        "function call has closing parens" in {
          goToDefinition()(
            """
              |Contract Action() {
              |  fn >>function<<() -> Bool {
              |    return true
              |  }
              |}
              |
              |Contract Main() {
              |  pub fn main(action: Action) -> () {
              |    let result = action.functio@@n()
              |  }
              |}
              |""".stripMargin
          )
        }

        "function call does not have closing parens" in {
          goToDefinitionSoft()(
            """
              |Contract Action() {
              |  fn >>function<<() -> Bool {
              |    return true
              |  }
              |}
              |
              |Contract Main() {
              |  pub fn main(action: Action) -> () {
              |    let result = action.functio@@n
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "unique contract name with duplicate function names" in {
        goToDefinition()(
          """
            |Contract ConA() {
            |  pub fn >>foo<<(boolean: Bool) -> () { }
            |}
            |
            |Contract ConB()  {
            |  pub fn foo(conA: ConA) -> () {
            |    conA.fo@@o(true)
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "external function exists in nested hierarchy" in {
      goToDefinition()(
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
            |    let result = action.functio@@n()
            |  }
            |}
            |""".stripMargin
      )
    }
  }

}
