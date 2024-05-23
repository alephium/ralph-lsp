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

class LoadDataBySelectorsCompletionSpec extends AnyWordSpec with Matchers {

  "suggest by data selectors (eg: `let result = object.funct@@`)" when {
    def testSuggestion(callingCode: String) = {
      val suggestions =
        suggest {
          s"""
             |Abstract Contract NotThat() {
             |
             |  // this function thought available is not available in scope, so it should not be suggest.
             |  fn notThatFunction() -> ()
             |
             |}
             |
             |Abstract Contract Parent() {
             |
             |  fn parentFunction1() -> ()
             |
             |  fn parentFunction2() -> Bool {
             |    return true
             |  }
             |
             |}
             |
             |Abstract Contract That() extends Parent() {
             |
             |  fn thatFunction1() -> ()
             |
             |  fn thatFunction2() -> Bool {
             |    return true
             |  }
             |
             |}
             |
             |$callingCode
             |""".stripMargin
        }

      val completions =
        suggestions.flatMap(_.toCompletion())

      val expected =
        List(
          Completion.Method(
            label = """thatFunction1() -> ()""",
            insert = """thatFunction1()""",
            detail = ""
          ),
          Completion.Method(
            label = """thatFunction2() -> Bool""",
            insert = """thatFunction2()""",
            detail = ""
          ),
          Completion.Method(
            label = """parentFunction1() -> ()""",
            insert = """parentFunction1()""",
            detail = ""
          ),
          Completion.Method(
            label = """parentFunction2() -> Bool""",
            insert = """parentFunction2()""",
            detail = ""
          )
        )

      completions.sortBy(_.label) shouldBe expected.sortBy(_.label)
    }

    "cursor is after the dot" in {
      testSuggestion(
        """
          |Contract Main() {
          |  fn main(that: That) -> () {
          |    let field = that.@@a
          |  }
          |}
          |""".stripMargin
      )
    }

    "cursor is after the dot and after the typed token" in {
      testSuggestion(
        """
          |Contract This() {
          |  fn thisFunction(that: That) -> () {
          |    let field = that.a@@
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
