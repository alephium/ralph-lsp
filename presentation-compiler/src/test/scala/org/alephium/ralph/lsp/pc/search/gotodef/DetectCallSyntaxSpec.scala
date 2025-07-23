// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class DetectCallSyntaxSpec extends AnyWordSpec with Matchers {

  "jump all non-variable definitions" when {
    "variable does not exist" when {
      "call is a value call" in {
        goToDefinitionSoft() {
          """
            |Contract >>variable<<() {}
            |event >>variable<<(a: Bool)
            |struct >>variable<< { a: Bool }
            |
            |Contract Test() {
            |  Contract >>variable<<() {}
            |  event >>variable<<(a: Bool)
            |  struct >>variable<< { a: Bool }
            |
            |  fn >>variable<<() -> () {}
            |
            |  let vari_able = 1
            |  variab@@le
            |}
            |""".stripMargin
        }
      }
    }
  }

  "jump only to the arguments" when {
    "call is a value call" in {
      goToDefinitionSoft() {
        """
          |Contract variable() {}
          |event variable(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(>>variable<<: Var) {
          |  Contract variable() {}
          |  event variable(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn variable() -> () {}
          |
          |  fn main(vari_able: Var) -> () {
          |    let vari_able = 1
          |    variab@@le
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "jump only the contracts and functions" when {
    "call is a reference call" in {
      goToDefinitionSoft() {
        """
          |Contract >>variable<<() {}
          |event variable(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(variable: Var) {
          |  Contract >>variable<<() {}
          |  event variable(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn >>variable<<() -> () {}
          |
          |  fn main(variable: Var) -> () {
          |    let variable = 1
          |    variab@@le()
          |  }
          |}
          |""".stripMargin
      }
    }

    "call is a reference call, followed by a method call" in {
      goToDefinitionSoft() {
        """
          |Contract >>variable<<() {}
          |event variable(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(variable: Var) {
          |  Contract >>variable<<() {}
          |  event variable(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn >>variable<<() -> () {}
          |
          |  fn main(variable: Var) -> () {
          |    let variable = 1
          |    variab@@le().method()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "jump only to the event" when {
    "call is an emit reference call" in {
      goToDefinitionSoft() {
        """
          |Contract variable() {}
          |event >>variable<<(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(variable: Var) {
          |  Contract variable() {}
          |  event >>variable<<(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn variable() -> () {}
          |
          |  fn main(variable: Var) -> () {
          |    let variable = 1
          |    emit variab@@le()
          |  }
          |}
          |""".stripMargin
      }
    }

    "call is an emit value call" in {
      goToDefinitionSoft() {
        """
          |Contract variable() {}
          |event >>variable<<(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(variable: Var) {
          |  Contract variable() {}
          |  event >>variable<<(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn variable() -> () {}
          |
          |  fn main(variable: Var) -> () {
          |    let variable = 1
          |    emit variab@@le
          |  }
          |}
          |""".stripMargin
      }
    }

    "call is a reference call, followed by a method call" in {
      goToDefinitionSoft() {
        """
          |Contract >>variable<<() {}
          |event variable(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(variable: Var) {
          |  Contract >>variable<<() {}
          |  event variable(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn >>variable<<() -> () {}
          |
          |  fn main(variable: Var) -> () {
          |    let variable = 1
          |    variab@@le().method()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "jump only to the contracts, arguments and variables" when {
    "call is a value call" in {
      goToDefinitionSoft() {
        """
          |Contract variable() {}
          |event variable(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(>>variable<<: Var) {
          |  Contract variable() {}
          |  event variable(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn variable() -> () {}
          |
          |  fn main(>>variable<<: Var) -> () {
          |    let >>variable<< = 1
          |    variab@@le.method()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "jump to only the variable definition" when {
    "call is value call" in {
      goToDefinitionSoft() {
        """
          |Contract variable() {}
          |event variable(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(>>variable<<: Var) {
          |  Contract variable() {}
          |  event variable(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn variable() -> () {}
          |
          |  fn main(>>variable<<: Var) -> () {
          |    let >>variable<< = 1
          |    variab@@le
          |  }
          |}
          |""".stripMargin
      }
    }

    "call is method call" in {
      goToDefinitionSoft() {
        """
          |Contract variable() {}
          |event variable(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(>>variable<<: Var) {
          |  Contract variable() {}
          |  event variable(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn variable() -> () {}
          |
          |  fn main(>>variable<<: Var) -> () {
          |    let >>variable<< = 1
          |    variab@@le.method()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "jump to only the struct definition" when {
    "call is struct constructor call" in {
      goToDefinitionSoft() {
        """
          |Contract variable() {}
          |event variable(a: Bool)
          |struct >>variable<< { a: Bool }
          |
          |Contract Test(variable: Var) {
          |  Contract variable() {}
          |  event variable(a: Bool)
          |  struct >>variable<< { a: Bool }
          |
          |  fn variable() -> () {}
          |
          |  fn main(variable: Var) -> () {
          |    let variable = 1
          |    variab@@le {}
          |  }
          |}
          |""".stripMargin
      }
    }

    "call is reference call with asset approval" in {
      goToDefinitionSoft() {
        """
          |Contract >>variable<<() {}
          |event variable(a: Bool)
          |struct variable { a: Bool }
          |
          |Contract Test(variable: Var) {
          |  Contract >>variable<<() {}
          |  event variable(a: Bool)
          |  struct variable { a: Bool }
          |
          |  fn >>variable<<() -> () {}
          |
          |  fn main(variable: Var) -> () {
          |    let variable = 1
          |    variab@@le{user -> 1}()
          |  }
          |}
          |""".stripMargin
      }
    }
  }

}
