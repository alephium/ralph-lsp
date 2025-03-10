// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToNamedVarSpec extends AnyWordSpec with Matchers {

  "return self" when {
    "variable definition is selected" in {
      goToDefinition()(
        """
          |Contract Test() {
          |
          |  pub fn function() -> () {
          |    let >>count@@er<< = 0
          |  }
          |}
          |""".stripMargin
      )
    }

    "duplicates exist" when {
      "first duplicate is selected" in {
        goToDefinition()(
          """
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let >>count@@er<< = 0
            |    let counter = 0
            |  }
            |}
            |""".stripMargin
        )
      }

      "second duplicate is selected" in {
        goToDefinition()(
          """
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let counter = 0
            |    let >>count@@er<< = 0
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }

}
