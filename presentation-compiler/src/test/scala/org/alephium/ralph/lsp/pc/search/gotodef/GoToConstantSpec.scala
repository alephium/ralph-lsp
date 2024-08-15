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

class GoToConstantSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "constant does not exist" in {
      goTo(
        """
          |Contract GoToConstant() {
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "constant exists" in {
      goTo(
        """
          |>>const MyConstant = 1<<
          |
          |Abstract Contract Parent() {
          |  >>const MyConstant = 1<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  >>const MyConstant = 0<<
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "duplicate constants exists" in {
      goTo(
        """
          |>>const MyConstant = 0<<
          |>>const MyConstant = 1<<
          |>>const MyConstant = 2<<
          |>>const MyConstant = 3<<
          |
          |Abstract Contract Parent() {
          |  >>const MyConstant = 2<<
          |  >>const MyConstant = 3<<
          |}
          |
          |Contract Child() extends Parent() {
          |
          |  >>const MyConstant = 0<<
          |  >>const MyConstant = 1<<
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "constant and the Contract have the same name" in {
      goTo(
        """
          |Abstract Contract MyConstant() {
          |
          |  >>const MyConstant = 2<<
          |  >>const MyConstant = 3<<
          |}
          |
          |Contract MyConstant() extends MyConstant() {
          |
          |  >>const MyConstant = 0<<
          |  >>const MyConstant = 1<<
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "only a global constant exists" in {
      goTo(
        """
          |>>const MyConstant = 0<<
          |
          |Contract Test() {
          |
          |  pub fn function() -> () {
          |    let my_constant = MyCo@@nstant
          |  }
          |}
          |""".stripMargin
      )
    }

    "constants with expression" in {
      goTo {
        """
          |const ONE = 1
          |const TWO = 2
          |>>const THREE = ONE + TWO<<
          |
          |Contract Test() {
          |  pub fn main() -> () {
          |     let three = THRE@@E
          |  }
          |}
          |""".stripMargin
      }
    }

    "constants is defined after its usage" in {
      goTo {
        """
          |Contract Test() {
          |  pub fn main() -> () {
          |     let one = ONE@@
          |  }
          |}
          |
          |>>const ONE = 1<<
          |""".stripMargin
      }
    }

    "constant is used within array type definition" when {
      "global constant" in {
        goTo {
          """
            |const SIZE@@ = 2
            |
            |Contract Test() {
            |  fn test() -> [U256; >>SIZE<<] {
            |    return [0; >>SIZE<<]
            |  }
            |}
            |""".stripMargin
        }
      }

      "local constant" in {
        goTo {
          """
            |Contract Test() {
            |
            |  const SIZE@@ = 2
            |
            |  fn test() -> [U256; >>SIZE<<] {
            |    return [0; >>SIZE<<]
            |  }
            |}
            |""".stripMargin
        }
      }
    }
  }

}
