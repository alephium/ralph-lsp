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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class GoToTypeIdStructUsageSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "no usage exists" in {
      goTo {
        """
          |struct Foo@@ {
          |  x: U256,
          |  y: U256
          |}
          |""".stripMargin
      }
    }
  }

  "return non-empty" when {
    "usage exists" when {
      "within itself" in {
        goTo {
          """
            |struct Foo@@ {
            |  x: U256,
            |  mut foo: >>Foo<<
            |}
            |""".stripMargin
        }
      }

      "within another struct" in {
        goTo {
          """
            |struct Foo@@ { x: U256 }
            |struct Bar { mut foo: >>Foo<< }
            |""".stripMargin
        }
      }

      "within a contract" in {
        goTo {
          """
            |struct Foo@@ { x: U256 }
            |
            |Contract Test(foo: >>Foo<<) {
            |
            |  fn function(foo: >>Foo<<) -> () {
            |    let foo = >>Foo<< { x: 1 }
            |  }
            |
            |}
            |""".stripMargin
        }
      }
    }
  }

}
