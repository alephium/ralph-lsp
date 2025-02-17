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

class GoToStructFieldSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "a struct field and an assignment value have duplicate names" when {
      "struct is defined external to the contract" in {
        goToDefinition() {
          """
            |struct MyStruct {
            |  structField: Bool
            |}
            |
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let copy = struct@@Field
            |  }
            |}
            |""".stripMargin
        }
      }

      "struct is defined within the contract" in {
        goToDefinitionSoft() {
          """
            |Contract Test() {
            |
            |  struct MyStruct {
            |    structField: Bool
            |  }
            |
            |  pub fn function() -> () {
            |    let copy = struct@@Field
            |  }
            |}
            |""".stripMargin
        }
      }
    }
  }

}
