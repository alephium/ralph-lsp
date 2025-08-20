// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToBuiltInUnitTests extends AnyWordSpec with Matchers {

  "jump to testEqual!" when {
    "strict" in {
      goToDefBuiltIn(
        code = """
            |Contract MyTest() {
            |
            |  test "simple test" {
            |    testEq@@ual!(2 + 3, 5)
            |  }
            |}
            |""".stripMargin,
        expected = Some("""fn >>testEqual!<<""")
      )
    }

    "soft" when {
      "no contract" ignore {
        goToDefBuiltIn(
          code = """
              |test "simple test" {
              |  testEq@@ual!(2 + 3, 5)
              |}
              |""".stripMargin,
          expected = Some("""fn >>testEqual!<<""")
        )
      }

      "contains syntax errors" ignore {
        goToDefBuiltIn(
          code = """
              |test "simple test" {
              |  testEq@@ual!(2 +
              |""".stripMargin,
          expected = Some("""fn >>testEqual!<<""")
        )
      }
    }
  }

}
