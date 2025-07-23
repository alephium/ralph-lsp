// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToStructFieldSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "a struct field and an assignment value have duplicate names" when {
      "struct is defined external to the contract" in {
        goToDefinition() {
          """
            |struct MyStruct {
            |  structField: Bool
            |}
            |
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let copy = struct@@Field
            |  }
            |}
            |""".stripMargin
        }
      }

      "struct is defined within the contract" in {
        goToDefinitionSoft() {
          """
            |Contract Test() {
            |
            |  struct MyStruct {
            |    structField: Bool
            |  }
            |
            |  pub fn function() -> () {
            |    let copy = struct@@Field
            |  }
            |}
            |""".stripMargin
        }
      }
    }
  }

}
