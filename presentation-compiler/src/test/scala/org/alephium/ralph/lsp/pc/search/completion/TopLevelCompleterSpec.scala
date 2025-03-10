// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class TopLevelCompleterSpec extends AnyWordSpec with Matchers {

  "suggest top level statements" when {
    "requested for the first line" in {
      val suggestions =
        suggest {
          """
            |@@
            |
            |Abstract Contract Dummy() { }
            |""".stripMargin
        }

      suggestions shouldBe TopLevelCompleter.suggest()
    }

    "requested for the second line" in {
      val suggestions =
        suggest {
          """
            |Abstract Contract Dummy() { }
            |
            |@@
            |""".stripMargin
        }

      suggestions shouldBe TopLevelCompleter.suggest()
    }
  }

}
