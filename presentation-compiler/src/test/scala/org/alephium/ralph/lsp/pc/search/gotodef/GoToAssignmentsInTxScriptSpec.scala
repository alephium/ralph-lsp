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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToAssignmentsInTxScriptSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "assigned variable does not exist" in {
      goTo(
        """
          |TxScript GoToAssignment() {
          |  counte@@r = counter + 1
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "assigned variables exist" in {
      goTo(
        """
          |TxScript GoToAssignment(>>counter: U256<<) {
          |  >>let mut counter = 0<<
          |  counte@@r = counter + 1
          |  for (>>let mut counter = 0<<; counter <= 4; counter = counter + 1) {
          |    counter = counter + 1
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
