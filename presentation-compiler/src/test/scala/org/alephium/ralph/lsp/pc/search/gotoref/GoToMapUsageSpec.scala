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

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToMapUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there is no map usage" in {
      goToReferences(
        """
          |Contract Test() {
          |
          |  mapping[Address, U256] counter@@s
          |
          |  pub fn function() -> () { }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "map has usages" in {
      goToReferences(
        """
          |Abstract Contract Parent() {
          |  mapping[Address, U256] counter@@s
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  pub fn function() -> Boolean {
          |    let value = >>counters<<[key]
          |    >>counters<<[key] = value + 1
          |    >>counters<<.insert!(depositor, key, 0)
          |    >>counters<<.remove!(depositRecipient, key)
          |    return >>counters<<.contains!(callerAddress!())
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
