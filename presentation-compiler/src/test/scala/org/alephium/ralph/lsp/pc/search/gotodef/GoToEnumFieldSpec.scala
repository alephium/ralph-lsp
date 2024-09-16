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

class GoToEnumFieldSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "enum type does not exist" in {
      goToDefinition(
        """
          |Contract MyContract() {
          |  pub fn function() -> () {
          |    let field = EnumType.Fie@@ld0
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return self" when {
    "enum field definition is selected" in {
      goToDefinition(
        """
          |// This parent is not inherited
          |Abstract Contract ParentNotUsed() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |enum EnumType {
          |  >>Field@@0 = 0<<
          |  Field1 = 1
          |}
          |""".stripMargin
      )
    }
  }

  "return non-empty" when {
    "user selects the first enum field" in {
      goToDefinition(
        """
          |// This parent is not inherited
          |Abstract Contract ParentNotUsed() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    Field1 = 1
          |  }
          |}
          |
          |enum EnumType {
          |  >>Field0 = 0<<
          |  Field1 = 1
          |}
          |
          |Abstract Contract Parent() {
          |
          |  enum EnumType {
          |    >>Field0 = 0<<
          |    Field1 = 1
          |  }
          |}
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum EnumType {
          |    >>Field0 = 0<<
          |    Field1 = 1
          |  }
          |
          |  pub fn function() -> () {
          |    let field0 = EnumType.Fie@@ld0
          |    let field1 = EnumType.Field1
          |  }
          |}
          |
          |enum EnumType {
          |  >>Field0 = 0<<
          |  Field1 = 1
          |}
          |""".stripMargin
      )
    }

    "user selects the second enum field" in {
      goToDefinition(
        """
          |enum EnumType {
          |  Field0 = 0
          |  >>Field1 = 1<<
          |}
          |
          |Abstract Contract Parent() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    >>Field1 = 1<<
          |  }
          |}
          |
          |
          |Contract MyContract() extends Parent() {
          |
          |  enum EnumType {
          |    Field0 = 0
          |    >>Field1 = 1<<
          |  }
          |
          |  pub fn function() -> () {
          |    let field0 = EnumType.Field0
          |    let field1 = EnumType.Fie@@ld1
          |  }
          |}
          |
          |enum EnumType {
          |  Field0 = 0
          |  >>Field1 = 1<<
          |}
          |""".stripMargin
      )
    }

    "there are duplicate enum types and fields" when {
      "user selects the first enum field" in {
        goToDefinition(
          """
            |enum EnumType {
            |  >>Field0 = 0<<
            |  Field1 = 1
            |}
            |
            |Abstract Contract Parent() {
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |}
            |
            |enum EnumType {
            |  >>Field0 = 0<<
            |  Field1 = 1
            |}
            |
            |Contract MyContract() extends Parent() {
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Fi@@eld0
            |    let field1 = EnumType.Field1
            |  }
            |}
            |""".stripMargin
        )
      }

      "user selects the second enum field" in {
        goToDefinition(
          """
            |enum EnumType {
            |  Field0 = 0
            |  >>Field1 = 1<<
            |}
            |
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Field0 = 0
            |    >>Field1 = 1<<
            |  }
            |
            |  enum EnumType {
            |    Field0 = 0
            |    >>Field1 = 1<<
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0
            |    let field1 = EnumType.Fi@@eld1
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "there are duplicate enum types with distinct fields" when {
      "user selects the first enum field" in {
        goToDefinition(
          """
            |enum EnumType {
            |  >>Field0 = 0<<
            |  Field1 = 1
            |}
            |
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    Field2 = 2
            |    Field3 = 3
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Fie@@ld0
            |    let field1 = EnumType.Field1
            |    let field2 = EnumType.Field2
            |    let field3 = EnumType.Field3
            |  }
            |}
            |""".stripMargin
        )
      }

      "user selects the third enum field" in {
        goToDefinition(
          """
            |Contract MyContract() {
            |
            |  enum EnumType {
            |    Field0 = 0
            |    Field1 = 1
            |  }
            |
            |  enum EnumType {
            |    >>Field2 = 2<<
            |    Field3 = 3
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0
            |    let field1 = EnumType.Field1
            |    let field2 = EnumType.Fie@@ld2
            |    let field3 = EnumType.Field3
            |  }
            |}
            |""".stripMargin
        )
      }

      "an enum field is selected that's implemented within a parent" in {
        goToDefinition(
          """
            |Abstract Contract Parent2() {
            |  enum EnumType {
            |    >>Field0 = 00<<
            |    Field3 = 3
            |  }
            |}
            |
            |Abstract Contract Parent1() extends Parent2() {
            |  enum EnumType {
            |    >>Field0 = 00<<
            |    Field3 = 3
            |  }
            |}
            |
            |Contract MyContract() extends Parent1() {
            |
            |  enum EnumType {
            |    >>Field0 = 0<<
            |    Field1 = 1
            |  }
            |
            |  pub fn function() -> () {
            |    let field0 = EnumType.Field0@@
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }

}
