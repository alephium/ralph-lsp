// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HoverVariableSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "variable does not exist" in {
      hover() {
        """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |    // varB does not exists
            |    let varA = v@@arB
            |  }
            |
            |}
            |""".stripMargin
      }
    }
  }

  "return variable with type" when {
    "hover on self" in {
      hover()(
        code = """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |
            |    let v@@arA = true
            |  }
            |
            |}
            |""".stripMargin,
        expected = "let varA: Bool"
      )
    }

    "variable exist" in {
      hover()(
        code = """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |
            |    let varA = true
            |    let varB = v@@arA
            |  }
            |
            |}
            |""".stripMargin,
        expected = "let varA: Bool"
      )
    }

    "variable has comment" in {
      hover()(
        code = """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |
            |    // This is a comment for varA
            |    let varA = true
            |    let varB = v@@arA
            |  }
            |
            |}
            |""".stripMargin,
        expected = "let varA: Bool"
      )
    }

    "variable is assigned with a function" in {
      hover()(
        code = """
            |Contract HoverTest() {
            |
            |  pub fn someFunction() -> U256 {
            |    return 1
            |  }
            |  pub fn function() -> () {
            |
            |    let varA = someFunction()
            |    let varB = v@@arA
            |  }
            |
            |}
            |""".stripMargin,
        expected = "let varA: U256"
      )
    }
  }

  "return variable with assignment" when {
    "type doesn't exist" in {
      hover()(
        code = """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |
            |    let varA = AAA
            |    let varB = v@@arA
            |  }
            |
            |}
            |""".stripMargin,
        expected = "let varA = AAA"
      )
    }

    "assignment function doesn't compile" in {
      hover()(
        code = """
            |Contract HoverTest() {
            |
            |  pub fn wrongFunction() -> U256 {
            |    return true
            |  }
            |
            |  pub fn function() -> () {
            |
            |    let varA = wrongFunction()
            |    let varB = v@@arA
            |  }
            |
            |}
            |""".stripMargin,
        expected = "let varA = wrongFunction()"
      )
    }
  }

}
