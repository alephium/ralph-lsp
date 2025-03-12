// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToStructSpec extends AnyWordSpec with Matchers {

  "return self" when {
    "strict-parseable" when {
      "struct exists" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruc@@t<< {
            |  structField: Bool
            |}
            |""".stripMargin
        }
      }

      "duplicate structs exist" in {
        goToDefinitionSoft() {
          """
            |{
            |  struct MyStruct {
            |    structField: Bool
            |  }
            |
            |  struct >>MyStruc@@t<< {
            |    structField: Bool
            |  }
            |}
            |""".stripMargin
        }
      }
    }

    "soft-parseable" when {
      "struct exists" in {
        goToDefinitionSoft() {
          """
            |struct >>MyStruc@@t<<
            |""".stripMargin
        }
      }

      "duplicate structs exist" in {
        goToDefinitionSoft() {
          """
            |{
            |  struct MyStruct
            |  struct >>MyStruc@@t<<
            |}
            |""".stripMargin
        }
      }
    }
  }

}
