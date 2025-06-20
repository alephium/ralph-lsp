// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HoverVariableSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable does not exist" in {
      hover() {
        """
            |Contract GoToTest() {
            |
            |  pub fn function() -> () {
            |    // varB does not exists
            |    let varA = v@@arB
            |  }
            |
            |}
            |""".stripMargin
      }
    }

    "variable exist" in {
      hover() {
        """
            |Contract GoToTest() {
            |
            |  pub fn function() -> () {
            |
            |    //Currently this test is only using the soft version
            |    //so the `TypeId` can't be found and return the all assignment.
            |
            |    let >>varA = true<<
            |    lev varB = v@@arA
            |  }
            |
            |}
            |""".stripMargin
      }
    }
  }

}
