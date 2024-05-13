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

class FunctionBodyCompleterSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "local params do not exist" in {
      val suggestions =
        suggest {
          """
            |Contract Test() {
            |  fn function() -> () {
            |    @@
            |  }
            |}
            |
            |""".stripMargin
        }

      suggestions shouldBe empty
    }
  }

  "return non-empty" when {
    "suggestions exist locally to the function" in {
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
              |
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
          Completion.Function(
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
            label = "mut index",
            insert = "index",
            detail = ""
          ),
          Completion.Variable(
            label = "enumField",
            insert = "enumField",
            detail = ""
          )
        )

      val actual =
        suggestions.flatMap(_.toCompletion())

      actual.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }
  }

}
