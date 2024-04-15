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

class GoToEventIdUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "event has no usage" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfe@@r(to: Address, amount: U256)
          |
          |  pub fn function() -> () {
          |
          |  }
          |}
          |
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "an event usage exists" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfe@@r(to: Address, amount: U256)
          |
          |  pub fn function() -> () {
          |    emit >>Transfer<<(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }

    "multiple usage exists" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfe@@r(to: Address, amount: U256)
          |
          |  pub fn function() -> () {
          |    emit >>Transfer<<(to, amount)
          |    for (let mut index = 0; index <= 4; index = index + 1) {
          |      emit >>Transfer<<(to, amount)
          |    }
          |    emit >>Transfer<<(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
