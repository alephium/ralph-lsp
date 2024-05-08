// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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
