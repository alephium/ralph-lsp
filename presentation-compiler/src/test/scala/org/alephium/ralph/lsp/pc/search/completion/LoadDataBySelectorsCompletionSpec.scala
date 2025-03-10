// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
