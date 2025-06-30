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
      }()
    }
  }

  "return variable with type" when {
    "hover on self" in {
      hover()(code = """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |
            |    let v@@arA = true
            |  }
            |
            |}
            |""".stripMargin)(
        expected = "let varA: Bool"
      )
    }

    "variable exist" in {
      hover()(code = """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |
            |    let varA = true
            |    let varB = v@@arA
            |  }
            |
            |}
            |""".stripMargin)(
        expected = "let varA: Bool"
      )
    }

    "variable has comment" in {
      hover()(code = """
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
            |""".stripMargin)(
        expected = "let varA: Bool"
      )
    }

    "variable is assigned with a function" in {
      hover()(code = """
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
            |""".stripMargin)(
        expected = "let varA: U256"
      )
    }

    "annonymous variable" in {
      hover()(code = """
        |Contract HoverTest() {
        |
        |  pub fn function() -> () {
        |    let @@_ = true
        |  }
        |}
        |""".stripMargin)(
        expected = "let _: Bool"
      )
    }

    // TODO: Support tuple
    "Tuple type" in {
      hover()(code = """
      |Contract HoverTest() {
      |  fn bar() -> (U256, Boolean) {
      |    return 1, false
      |  }
      |  pub fn function() -> () {
      |    let (varA, varB) = bar()
      |    let varC = v@@arA
      |  }
      |}""".stripMargin)(
        // TODO: Should be: "let varA: U256"
      )
    }

    // TODO: Support variable from contract
    "variable assignment from contract " in {
      hover()(code = """

        |Contract Bar(id: U256) {
        |  pub fn func() -> U256 {
        |  }
        |}
        |
        |Contract Foo2() {
        |  pub fn foo() -> () {
        |    let b@@ar = Bar(1)
        |  }
        |}
        |""".stripMargin)(
        // TODO: Should be: "let bar: Bar"
        expected = "let bar = Bar(1)"
      )
    }
  }

  "mutable variable" when {
    "code compiles" in {
      hover()(code = """
        |Contract HoverTest() {
        |
        |  pub fn function() -> () {
        |    let mut index = 0
        |     while (index <= 4) {
        |       bar(in@@dex)
        |       index += 1
        |     }
        |  }
        |}
        |""".stripMargin)(
        "let mut index: U256"
      )
    }

    "hover self" in {
      hover()(code = """
        |Contract HoverTest() {
        |
        |  pub fn function() -> () {
        |    let mut i@@ndex = 0
        |  }
        |}
        |""".stripMargin)(
        "let mut index: U256"
      )
    }

    "code doesn't compile" in {
      hover()(code = """
        |Contract HoverTest() {
        |
        |  pub fn function() -> () {
        |    let mut index = 0
        |     while (i@@ndex)
        |  }
        |}
        |""".stripMargin)(
        // Type can't be found as it doesn't compile
        "let mut index = 0"
      )
    }
  }

  "return variable with assignment" when {
    "type doesn't exist" in {
      hover()(code = """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |
            |    let varA = AAA
            |    let varB = v@@arA
            |  }
            |
            |}
            |""".stripMargin)(
        expected = "let varA = AAA"
      )
    }

    "assignment function doesn't compile" in {
      hover()(code = """
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
            |""".stripMargin)(
        expected = "let varA = wrongFunction()"
      )
    }
  }

}
