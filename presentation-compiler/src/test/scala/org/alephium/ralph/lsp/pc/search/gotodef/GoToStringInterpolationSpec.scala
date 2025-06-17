// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToStringInterpolationSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "Strict-AST" when {
      "BString is string interpolated" in {
        goToDefinition()(
          """
            |Contract Test() {
            |
            |  fn test() -> () {
            |    let message = b`some debug message`
            |    let log = b`Info: ${messag@@e}`
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }

    "SoftAST" when {
      "BString is string interpolated" when {
        "within a block" in {
          goToDefinitionSoft()(
            """
              |{
              |  let message = b`some debug message`
              |  let log = b`Info: ${messag@@e}`
              |}
              |""".stripMargin
          )
        }

        "without block" in {
          goToDefinitionSoft()(
            """
              |let message = b`some debug message`
              |let log = b`Info: ${messag@@e}`
              |""".stripMargin
          )
        }
      }
    }
  }

  "return non-empty" when {
    "Strict-AST" when {
      "Debug event contains string interpolation" in {
        goToDefinition()(
          """
            |Contract Test() {
            |
            |  fn test() -> () {
            |    let >>message<< = b`some debug message`
            |    emit Debug(`Info: ${messag@@e}`)
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }

    "SoftAST" when {
      "nested interpolated strings" when {
        "within a block" in {
          goToDefinitionSoft()(
            """
              |{
              |  let ERROR_ID = 404
              |  let >>message<< = `Error: ${ERROR_ID}. Message: Some debug message`
              |  let log = `Log: ${messag@@e}`
              |}
              |""".stripMargin
          )
        }

        "without block" in {
          goToDefinitionSoft()(
            """
              |let ERROR_ID = 404
              |let >>message<< = `Error: ${ERROR_ID}. Message: Some debug message`
              |let log = `Log: ${messag@@e}`
              |""".stripMargin
          )
        }
      }
    }
  }

}
