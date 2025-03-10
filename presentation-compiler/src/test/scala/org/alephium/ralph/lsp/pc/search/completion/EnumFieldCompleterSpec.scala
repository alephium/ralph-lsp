// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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

    "global enum" should {
      "list enum fields" in {
        val suggestions =
          suggest {
            """
              |enum MyEnum {
              |  ONE = 1
              |  TWO = 2
              |}
              |
              |Contract Test() {
              |
              |  fn main() -> () {
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
              label = "ONE",
              insert = "ONE",
              detail = ""
            ),
            Completion.EnumMember(
              label = "TWO",
              insert = "TWO",
              detail = ""
            )
          )

        actual.sortBy(_.label) shouldBe expected.sortBy(_.label)
      }
    }
  }

}
