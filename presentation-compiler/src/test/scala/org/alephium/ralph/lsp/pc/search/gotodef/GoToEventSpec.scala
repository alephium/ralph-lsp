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

class GoToEventSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "event does not exists" in {
      goTo(
        """
          |Contract Test() {
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "an event exists" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |
          |  event TransferNotUsed(to: Address, amount: U256)
          |
          |  >>event Transfer(to: Address, amount: U256)<<
          |
          |}
          |
          |Contract Test() extends Parent() {
          |
          |  >>event Transfer(to: Address, amount: U256)<<
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }

    "duplicate events exist" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |
          |  >>event Transfer(amount: U256)<<
          |  >>event Transfer(to: Address, amount: U256)<<
          |  event TransferNotUsed(to: Address, amount: U256)
          |  >>event Transfer(to: Address)<<
          |
          |}
          |
          |Contract Test() extends Parent() {
          |
          |  >>event Transfer(to: Address, amount: U256)<<
          |  >>event Transfer(amount: U256)<<
          |  >>event Transfer(to: Address)<<
          |
          |  pub fn function() -> () {
          |    emit Transfe@@r(to, amount)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
