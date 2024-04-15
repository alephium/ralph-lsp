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

/** Test for go to function */
class GoToFunctionUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "function calls do not exist" in {
      goTo(
        """
          |Contract MyContract(interface: MyInterface) {
          |
          |  // function_a has no local calls
          |  pub fn @@function_a(boolean: Bool) -> () {
          |
          |  }
          |
          |  pub fn function_b(boolean: Bool) -> () {
          |    let call = function_c()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "function calls exist" in {
      goTo(
        """
          |Contract MyContract(interface: MyInterface) {
          |
          |  // function_a is clicked and it has 5 call
          |  pub fn @@function_a(boolean: Bool) -> () {
          |    let call1 = >>function_a(true)<<
          |    >>function_a(false)<<
          |  }
          |
          |  pub fn function_b(boolean: Bool) -> () {
          |    let call2 = >>function_a(true)<<
          |    >>function_a(false)<<
          |    let call3 = >>function_a(false)<<
          |    >>function_a(true)<<
          |  }
          |
          |  pub fn function_c(boolean: Bool) -> () {
          |    let call4 = >>function_a(false)<<
          |    for (let mut index = 0; index <= 4; index = index + 1) {
          |      >>function_a(true)<<
          |    }
          |    let call5 = >>function_a(true)<<
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
