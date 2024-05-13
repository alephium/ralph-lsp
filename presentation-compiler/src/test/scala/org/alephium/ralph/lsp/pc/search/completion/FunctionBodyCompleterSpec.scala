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
    "arguments exist" in {
      val suggestions =
        suggest {
          """
              |Contract Test() {
              |  fn function(bool: Bool,
              |              int: U256) -> () {
              |    let variable = true
              |    @@
              |    // this should not get suggested since it's after the completion request
              |    let variable_after = false
              |  }
              |}
              |
              |""".stripMargin
        }

      val expected =
        Seq(
          Completion.Field(
            label = "bool: Bool",
            insert = "bool",
            detail = ""
          ),
          Completion.Field(
            label = "int: U256",
            insert = "int",
            detail = ""
          ),
          // TODO: Also provide variables type information in suggestion.
          Completion.Variable(
            label = "variable",
            insert = "variable",
            detail = ""
          )
        )

      val actual =
        suggestions.flatMap(_.toCompletion())

      expected shouldBe actual
    }
  }

}
