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

class GoToEnumTypeSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "enum type does not exist" in {
      goToDefinition(
        """
          |Contract MyContract() {
          |  pub fn function() -> () {
          |    let go_to_enum = Enum@@Type.Field0
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "user selects the enum type of the first field" in {
      goToDefinition(
        """
          |enum >>EnumType<< {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |
          |Abstract Contract Parent() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let go_to_enum1 = Enum@@Type.Field0
          |    let go_to_enum2 = EnumType.Field1
          |  }
          |}
          |
          |enum >>EnumType<< {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |
          |enum NotThis {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |""".stripMargin
      )
    }

    "user selects the enum type of the second field" in {
      goToDefinition(
        """
          |enum >>EnumType<< {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |
          |Abstract Contract Parent() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |enum >>EnumType<< {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let go_to_enum1 = EnumType.Field0
          |    let go_to_enum2 = EnumTy@@pe.Field1
          |  }
          |}
          |""".stripMargin
      )
    }

    "there are multiple enum types with duplicate names" in {
      goToDefinition(
        """
          |enum >>EnumType<< {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |
          |Abstract Contract Parent2() {
          |
          |  enum EnumTypeNotUsed {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |enum >>EnumType<< {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |
          |Abstract Contract Parent1() extends Parent2() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  enum EnumTypeNotUsed {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |enum >>EnumType<< {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |
          |Contract MyContract() extends Parent1() {
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  enum >>EnumType<< {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let go_to_enum = Enu@@mType.Field0
          |  }
          |}
          |
          |enum >>EnumType<< {
          |  Field0 = 0
          |  Field1 = 1
          |}
          |""".stripMargin
      )
    }
  }

}
