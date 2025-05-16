// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.inlayhints

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InlayHintsSingleVariableSpec extends AnyWordSpec with Matchers {

  /**
   * **********************************
   * Test cases: Named single variable.
   * **********************************
   */
  "named variable" should {
    "return empty" when {
      "the range position is at the end" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let one = 1
            |  }
            |}
            |@@
            |""".stripMargin
        )
      }

      "range position is within the contract" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let one = 1
            |  }
            |  @@
            |}
            |""".stripMargin
        )
      }

      "range position is within the function" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let one = 1
            |    @@
            |  }
            |}
            |""".stripMargin
        )
      }

      "range position is within the variable" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let one = 1  @@
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "return non-empty" when {
      "the range position is at the root" in {
        inlayHints(
          """
            |@@
            |Contract Test() {
            |  fn test() -> () {
            |    let one>>: U256<< = 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "range position is within the contract" in {
        inlayHints(
          """
            |Contract Test() {
            |  @@
            |  fn test() -> () {
            |    let one>>: U256<< = 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "range position is within the function" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    @@
            |    let one>>: U256<< = 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "range position is within the variable" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let @@ one>>: U256<< = 1
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }

  /**
   * ************************************
   * Test cases: Unnamed single variable.
   * ************************************
   */
  "anonymous variable" should {
    "return empty" when {
      "the range position is at the end" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let _ = 1
            |  }
            |}
            |@@
            |""".stripMargin
        )
      }

      "range position is within the contract" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let _ = 1
            |  }
            |  @@
            |}
            |""".stripMargin
        )
      }

      "range position is within the function" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let _ = 1
            |    @@
            |  }
            |}
            |""".stripMargin
        )
      }

      "range position is within the variable" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let _ = 1  @@
            |  }
            |}
            |""".stripMargin
        )
      }
    }

    "return non-empty" when {
      "the range position is at the root" in {
        inlayHints(
          """
            |@@
            |Contract Test() {
            |  fn test() -> () {
            |    let _>>: U256<< = 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "range position is within the contract" in {
        inlayHints(
          """
            |Contract Test() {
            |  @@
            |  fn test() -> () {
            |    let _>>: U256<< = 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "range position is within the function" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    @@
            |    let _>>: U256<< = 1
            |  }
            |}
            |""".stripMargin
        )
      }

      "range position is within the variable" in {
        inlayHints(
          """
            |Contract Test() {
            |  fn test() -> () {
            |    let @@ _>>: U256<< = 1
            |  }
            |}
            |""".stripMargin
        )
      }
    }
  }

}
