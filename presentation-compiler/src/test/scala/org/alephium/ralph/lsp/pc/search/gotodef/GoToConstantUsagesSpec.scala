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
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToConstantUsagesSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "constant has not usage" in {
      goTo(
        """
          |Contract GoToConstant() {
          |
          |  const MyCons@@tant = 0
          |
          |  pub fn function() -> () {
          |
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "constant has multiple usages" when {
      def doTest(contractName: String): Assertion =
        goTo(
          s"""
             |Contract $contractName() {
             |
             |  const MyCons@@tant = 0
             |  const MyConstant_B = 1
             |
             |  pub fn function() -> () {
             |    let my_constant = >>MyConstant<<
             |    let my_constant2 = >>MyConstant<<
             |    let my_constant3 = MyConstant_B
             |    for (let mut index = 0; index <= 4; index = index + 1) {
             |      let my_constant4 = >>MyConstant<<
             |      let my_constant5 = MyConstant_B
             |    }
             |  }
             |}
             |""".stripMargin
        )

      "constant and contract have the same ID" in {
        // the constant name is also "MyConstant"
        doTest(contractName = "MyConstant")
      }

      "constant and contract have unique IDs" in {
        doTest(contractName = "MyContract")
      }

    }
  }

}
