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

class GoToMapSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "map does not exist" in {
      goTo(
        """
          |Contract Test() {
          |
          |  pub fn function() -> () {
          |    let value = counter@@s[key]
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "map value is extracted" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |  >>mapping[Address, U256] counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  >>mapping[Address, U256] counters<<
          |
          |  pub fn function() -> () {
          |    let value = counter@@s[key]
          |  }
          |}
          |""".stripMargin
      )
    }

    "map value is set" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |  >>mapping[Address, U256] counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  >>mapping[Address, U256] counters<<
          |
          |  pub fn function() -> () {
          |    counter@@s[key] = value + 1
          |  }
          |}
          |""".stripMargin
      )
    }

    "map is inserted" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |  >>mapping[Address, U256] counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  >>mapping[Address, U256] counters<<
          |
          |  pub fn function() -> () {
          |    counter@@s.insert!(depositor, key, 0)
          |  }
          |}
          |""".stripMargin
      )
    }

    "map item is remove" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |  >>mapping[Address, U256] counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  >>mapping[Address, U256] counters<<
          |
          |  pub fn function() -> () {
          |    counter@@s.remove!(depositRecipient, key)
          |  }
          |}
          |""".stripMargin
      )
    }

    "map is checked for contains" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |  >>mapping[Address, U256] counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  >>mapping[Address, U256] counters<<
          |
          |  pub fn function() -> () {
          |    counter@@s.contains!(callerAddress!())
          |  }
          |}
          |""".stripMargin
      )
    }

    "map function is returned" in {
      goTo(
        """
          |Abstract Contract Parent() {
          |  >>mapping[Address, U256] counters<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  >>mapping[Address, U256] counters<<
          |
          |  pub fn function() -> Bool {
          |    return counter@@s.contains!(callerAddress!())
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
