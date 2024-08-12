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

import org.alephium.ralph.Ast
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class FunctionBodyCompleterSpec extends AnyWordSpec with Matchers {

  "suggest non-empty" when {
    "suggestions exist due to declarations local to the function" in {
      val suggestions =
        suggest {
          """
            |Contract Test(templateBool: Bool) {
            |
            |  event TransferNotUsed(to: Address, amount: U256)
            |
            |  const MyConstant = 1
            |
            |  enum EnumType {
            |    Field0 = 0
            |    Field1 = 1
            |  }
            |
            |  fn function(bool: Bool,
            |              int: U256) -> () {
            |    let variable = true
            |    for (let mut index = 0; index <= 4; index = index + 1) {
            |      let enumField = EnumType.Field1
            |    }
            |    @@
            |    // the following should not get suggested since it's after the completion request
            |    let variable_after = false
            |    for (let mut index_after = 0; index_after <= 4; index_after = index_after + 1) {
            |      let enumField_after = EnumType.Field1
            |    }
            |  }
            |}
            |""".stripMargin
        }

      val expected =
        Seq(
          Completion.Property( // Property because it's a template argument
            label = "templateBool: Bool",
            insert = "templateBool",
            detail = ""
          ),
          Completion.Event(
            label = "TransferNotUsed",
            insert = "TransferNotUsed",
            detail = "event TransferNotUsed(to: Address, amount: U256)"
          ),
          Completion.Constant(
            label = "MyConstant",
            insert = "MyConstant",
            detail = ""
          ),
          Completion.Enum(
            label = "EnumType",
            insert = "EnumType",
            detail = ""
          ),
          Completion.Method(
            label = "function(bool: Bool, int: U256) -> ()",
            insert = "function()",
            detail = ""
          ),
          Completion.Field( // Field because it's a function argument
            label = "bool: Bool",
            insert = "bool",
            detail = ""
          ),
          Completion.Field( // Field because it's a function argument
            label = "int: U256",
            insert = "int",
            detail = ""
          ),
          // TODO: Also provide variables type information in suggestion.
          Completion.Variable(
            label = "variable",
            insert = "variable",
            detail = ""
          ),
          Completion.Variable(
            label = "index",
            insert = "index",
            detail = "mut index"
          ),
          Completion.Variable(
            label = "enumField",
            insert = "enumField",
            detail = ""
          ),
          Completion.Keyword(
            label = "let",
            insert = "let ",
            detail = ""
          )
        )

      val actual =
        suggestions.flatMap(_.toCompletion())

      actual should contain allElementsOf expected
    }

    "suggestions exist due to inheritance" in {
      val suggestions =
        suggest {
          """
            |Abstract Contract Parent(parentTemplateBool: Bool) {
            |
            |  event TransferNotUsed(to: Address, amount: U256)
            |
            |  const MyConstant = 1
            |
            |  enum EnumType {
            |    Field0 = 0
            |    Field1 = 1
            |  }
            |
            |  fn function_parent(bool: Bool,
            |                     int: U256) -> () {
            |   let variable_parent = true
            |   // these are not visible outside
            |   for (let mut index = 0; index <= 4; index = index + 1) {
            |      let enumField = EnumType.Field1
            |    }
            |  }
            |}
            |
            |Contract Test(templateBool: Bool) extends Parent(templateBool) {
            |
            |  fn function(bool: Bool,
            |              int: U256) -> () {
            |    let variable = true
            |    @@
            |    // the following should not get suggested since it's after the completion request
            |    let variable_after = false
            |
            |  }
            |}
            |""".stripMargin
        }

      val expected =
        Seq(
          Completion.Property( // Property because it's a template argument
            label = "parentTemplateBool: Bool",
            insert = "parentTemplateBool",
            detail = ""
          ),
          Completion.Property( // Property because it's a template argument
            label = "templateBool: Bool",
            insert = "templateBool",
            detail = ""
          ),
          Completion.Event(
            label = "TransferNotUsed",
            insert = "TransferNotUsed",
            detail = "event TransferNotUsed(to: Address, amount: U256)"
          ),
          Completion.Constant(
            label = "MyConstant",
            insert = "MyConstant",
            detail = ""
          ),
          Completion.Enum(
            label = "EnumType",
            insert = "EnumType",
            detail = ""
          ),
          Completion.Method(
            label = "function(bool: Bool, int: U256) -> ()",
            insert = "function()",
            detail = ""
          ),
          Completion.Method(
            label = "function_parent(bool: Bool, int: U256) -> ()",
            insert = "function_parent()",
            detail = ""
          ),
          Completion.Field( // Field because it's a function argument
            label = "bool: Bool",
            insert = "bool",
            detail = ""
          ),
          Completion.Field( // Field because it's a function argument
            label = "int: U256",
            insert = "int",
            detail = ""
          ),
          // TODO: Also provide variables type information in suggestion.
          Completion.Variable(
            label = "variable",
            insert = "variable",
            detail = ""
          ),
          // Also provide type information
          Completion.Class(
            label = "Test",
            insert = "Test",
            detail = ""
          ),
          Completion.Class(
            label = "Parent",
            insert = "Parent",
            detail = ""
          ),
          // Test for one type information from dependency
          Completion.Class(
            label = "INFT",
            insert = "INFT",
            detail = ""
          ),
          Completion.Keyword(
            label = "let",
            insert = "let ",
            detail = ""
          )
        )

      val actual =
        suggestions.flatMap(_.toCompletion())

      actual should contain allElementsOf expected
    }

    "after variable assignment" in {
      val suggestions =
        suggest {
          """
          |Contract Test() {
          |
          |  fn aFunction() -> () {
          |    let aVariable = true
          |    let copyVariable = a@@
          |  }
          |
          |}
          |""".stripMargin
        }

      val actual =
        suggestions.flatMap(_.toCompletion())

      val expected =
        Array(
          Completion.Method(
            label = "aFunction() -> ()",
            insert = "aFunction()",
            detail = ""
          ),
          Completion.Variable(
            label = "aVariable",
            insert = "aVariable",
            detail = ""
          ),
          Completion.Keyword(
            label = "let",
            insert = "let ",
            detail = ""
          )
        )

      actual should contain allElementsOf expected
    }

    "within nested for loops" in {
      val suggestions =
        suggest {
          """
            |Contract Test() {
            |
            |  fn aFunction() -> () {
            |    let mut counter = 1
            |    for (let mut index1 = 0; index1 <= 4; index1 = index2 + 1) {
            |      counter = counter + 1
            |      for (let mut index2 = 0; index1 <= 4; index2 = i@@ + 1) {
            |        counter = counter + 1
            |      }
            |    }
            |  }
            |
            |}
            |""".stripMargin
        }

      val actual =
        suggestions.flatMap(_.toCompletion())

      val expected =
        Array(
          Completion.Method(
            label = "aFunction() -> ()",
            insert = "aFunction()",
            detail = ""
          ),
          Completion.Variable(
            label = "counter",
            insert = "counter",
            detail = "mut counter"
          ),
          Completion.Variable(
            label = "counter",
            insert = "counter",
            detail = "mut counter"
          ),
          Completion.Variable(
            label = "index1",
            insert = "index1",
            detail = "mut index1"
          ),
          Completion.Variable(
            label = "index2",
            insert = "index2",
            detail = "mut index2"
          ),
          // Also provide type information
          Completion.Class(
            label = "Test",
            insert = "Test",
            detail = ""
          ),
          // Test for one type information from dependency
          Completion.Class(
            label = "INFT",
            insert = "INFT",
            detail = ""
          ),
          Completion.Keyword(
            label = "let",
            insert = "let ",
            detail = ""
          )
        )

      actual should contain allElementsOf expected
    }

    "map definitions exist" in {
      val suggestions =
        suggest {
          """
            |Abstract Contract Parent2() {
            |  mapping[Address, Bool] mapParent2
            |}
            |
            |Abstract Contract Parent1() extends Parent2() {
            |  mapping[Address, ByteVec] mapParent1
            |}
            |
            |Contract Child() extends Parent1() {
            |  mapping[Address, U256] mapChild
            |
            |  fn function() -> () {
            |    let mapReference = map@@
            |  }
            |
            |}
            |""".stripMargin
        }

      val actual =
        suggestions
          .collect {
            case map: Suggestion.MapDef => map
          }
          .flatMap(_.toCompletion())

      val expected =
        List(
          Completion.Property(
            label = "mapParent2: Map[Address, Bool]",
            insert = "mapParent2",
            detail = ""
          ),
          Completion.Property(
            label = "mapParent1: Map[Address, ByteVec]",
            insert = "mapParent1",
            detail = ""
          ),
          Completion.Property(
            label = "mapChild: Map[Address, U256]",
            insert = "mapChild",
            detail = ""
          )
        )

      actual.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }

    "global constants exist" in {
      val suggestions =
        suggest {
          """
            |const ONE = 2
            |const TWO = 2
            |
            |Contract Foo() {
            |
            |  pub fn test() -> () {
            |    @@
            |  }
            |}
            |""".stripMargin
        }

      val actual =
        suggestions
          .collect {
            case constants: Suggestion.ConstantVarDef =>
              constants
          }
          .flatMap(_.toCompletion())

      val expected =
        List(
          Completion.Constant(
            label = "ONE",
            insert = "ONE",
            detail = ""
          ),
          Completion.Constant(
            label = "TWO",
            insert = "TWO",
            detail = ""
          )
        )

      actual.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }

    "global enums exist" in {
      val suggestions =
        suggest {
          """
            |enum GlobalEnumTop {
            |  ONE = 1
            |  TWO = 2
            |}
            |
            |Contract Foo() {
            |
            |  enum LocalEnum {
            |    THREE = 3
            |    FOUR = 4
            |  }
            |
            |  pub fn test() -> () {
            |    @@
            |  }
            |}
            |
            |enum GlobalEnumBottom {
            |  FIVE = 5
            |  SIX = 6
            |}
            |""".stripMargin
        }

      val actual =
        suggestions
          .collect {
            case enums: Suggestion.EnumDef =>
              enums

            case enumCreatedTypes: Suggestion.CreatedType if enumCreatedTypes.node.source.tree.ast.isInstanceOf[Ast.EnumDef[_]] =>
              enumCreatedTypes
          }
          .flatMap(_.toCompletion())

      // FIXME: Global enums should not be suggested as Classes. This is the case right now because
      //        All `GlobalDefinition`s that contain `typeId` are returned as Suggestion.CreatedInstance.
      val expected =
        List(
          Completion.Enum(
            label = "LocalEnum",
            insert = "LocalEnum",
            detail = ""
          ),
          Completion.Class(
            label = "GlobalEnumTop",
            insert = "GlobalEnumTop",
            detail = ""
          ),
          Completion.Class(
            label = "GlobalEnumBottom",
            insert = "GlobalEnumBottom",
            detail = ""
          )
        )

      actual.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }
  }

  "TxScript" should {
    "enable code completion within the main function" in {
      val suggestions =
        suggest {
          """
            |TxScript MyScript {
            |  let mut counterMain = 0
            |  counterMain = cou@@nterMain + 1
            |}
            |""".stripMargin
        }

      val actual =
        suggestions.flatMap(_.toCompletion())

      val expected =
        List(
          // Selected a random type from dependency
          Completion.Class(
            label = "INFT",
            insert = "INFT",
            detail = ""
          ),
          // Selected a random keyword
          Completion.Keyword(
            label = "let",
            insert = "let ",
            detail = ""
          ),
          // Local variables are suggested
          Completion.Variable(
            label = "counterMain",
            insert = "counterMain",
            detail = "mut counterMain"
          )
        )

      actual should contain allElementsOf expected
    }
  }

}
