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

package org.alephium.ralph.lsp.pc.search.completion

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class EnumFieldCompleterSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "Enum does not exist" in {
      val suggestions =
        suggest {
          """
            |Contract Test() {
            |
            |  fn function() -> () {
            |    let field = MyEnum.F@@
            |  }
            |
            |}
            |""".stripMargin
        }

      suggestions shouldBe empty
    }

    "Another enum exists, but the requested enum does not" in {
      // Test that only targeted enum fields are suggested.
      val suggestions =
        suggest {
          """
            |Contract Test() {
            |
            |  enum EnumNotAccessed {
            |    Field = 1
            |  }
            |
            |  fn function() -> () {
            |    let field = MyEnum.F@@
            |  }
            |
            |}
            |""".stripMargin
        }

      suggestions shouldBe empty
    }
  }

  "return non-empty" when {
    "enum fields are defined" in {
      val suggestions =
        suggest {
          """
            |Contract Child() {
            |  enum MyEnum {
            |    FieldOne = 1
            |    FieldTwo = 2
            |    FieldThree = 3
            |  }
            |
            |  // This one is not used
            |  enum EnumNotAccessed {
            |    Field = 1
            |  }
            |
            |  fn function() -> () {
            |    let field = MyEnum.F@@
            |  }
            |}
            |""".stripMargin
        }

      val actual =
        suggestions
          .collect {
            case fields: Suggestion.EnumFields => fields
          }
          .flatMap(_.toCompletion())

      val expected =
        List(
          Completion.EnumMember(
            label = "FieldOne",
            insert = "FieldOne",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldTwo",
            insert = "FieldTwo",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldThree",
            insert = "FieldThree",
            detail = ""
          )
        )

      actual.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }

    "enum fields are defined with duplicate names" in {
      val suggestions =
        suggest {
          """
            |Contract Child() {
            |
            |  enum MyEnum {
            |    FieldOne = 1
            |    FieldTwo = 2
            |    FieldThree = 3
            |  }
            |
            |  // Duplicate MyEnum
            |  enum MyEnum {
            |    // Duplicate FieldThree
            |    FieldThree = 3
            |    FieldFour = 4
            |  }
            |
            |  // This one is not used
            |  enum EnumNotAccessed {
            |    Field = 1
            |  }
            |
            |  fn function() -> () {
            |    let field = MyEnum.F@@
            |  }
            |}
            |""".stripMargin
        }

      val actual =
        suggestions
          .collect {
            case fields: Suggestion.EnumFields => fields
          }
          .flatMap(_.toCompletion())

      val expected =
        List(
          Completion.EnumMember(
            label = "FieldOne",
            insert = "FieldOne",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldTwo",
            insert = "FieldTwo",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldThree",
            insert = "FieldThree",
            detail = ""
          ),
          Completion.EnumMember( // Duplicate is suggested
            label = "FieldThree",
            insert = "FieldThree",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldFour",
            insert = "FieldFour",
            detail = ""
          )
        )

      actual.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }

    "enum fields are defined and duplicated in parents" in {
      val suggestions =
        suggest {
          """
            |Abstract Contract Parent1() {
            |  // Duplicate MyEnum
            |  enum MyEnum {
            |    // Duplicate FieldThree
            |    FieldThree = 3
            |    FieldFour = 4
            |  }
            |}
            |
            |Abstract Contract Parent2() extends Parent1() {
            |  enum MyEnum {
            |    FieldOne = 1
            |    FieldTwo = 2
            |    FieldThree = 3
            |  }
            |}
            |
            |Contract Child() extends Parent2() {
            |
            |  // This one is not used
            |  enum EnumNotAccessed {
            |    Field = 1
            |  }
            |
            |  fn function() -> () {
            |    let field = MyEnum.F@@
            |  }
            |}
            |""".stripMargin
        }

      val actual =
        suggestions
          .collect {
            case fields: Suggestion.EnumFields => fields
          }
          .flatMap(_.toCompletion())

      val expected =
        List(
          Completion.EnumMember(
            label = "FieldOne",
            insert = "FieldOne",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldTwo",
            insert = "FieldTwo",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldThree",
            insert = "FieldThree",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldThree", // Duplicate is suggested
            insert = "FieldThree",
            detail = ""
          ),
          Completion.EnumMember(
            label = "FieldFour",
            insert = "FieldFour",
            detail = ""
          )
        )

      actual.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }
  }

}
