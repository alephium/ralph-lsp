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

class GoToEventFieldUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "first event field has no usage" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfer(t@@o: Address, amount: U256)
          |
          |  pub fn function() -> () {
          |
          |  }
          |}
          |
          |""".stripMargin
      )
    }

    "second event field has no usage" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
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

  "return non-empty for an event field" when {
    "it has a usage" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
          |
          |  pub fn function() -> () {
          |    emit Transfer(to, >>someAmount<<)
          |  }
          |}
          |""".stripMargin
      )
    }

    "it has multiple usages" in {
      goTo(
        """
          |Contract Test() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
          |
          |  pub fn function() -> () {
          |    emit Transfer(to, >>amount1<<)
          |    for (let mut index = 0; index <= 4; index = index + 1) {
          |      emit Transfer(to, >>amount2<<)
          |    }
          |    emit Transfer(to, >>amount3<<)
          |  }
          |}
          |""".stripMargin
      )
    }

    "there is inheritance" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |
          |  event Transfer(to: Address, a@@mount: U256)
          |
          |  fn function0() -> () {
          |    emit Transfer(to, >>amount1<<)
          |  }
          |}
          |
          |Contract Parent1() extends Parent() {
          |
          |  pub fn function1() -> () {
          |    emit Transfer(to, >>amount1<<)
          |  }
          |}
          |
          |Contract Child() extends Parent1() {
          |
          |  pub fn function2() -> () {
          |    emit Transfer(to, >>amount1<<)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
