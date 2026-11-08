// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.inlayhints

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InlayHintsSingleVariableSpec extends AnyWordSpec with Matchers {

  "primitive and contract names" should {
    "produce one primitive ByteVec hint linked to the built-in type" in {
      val hints = inlayHints(
        """
          |@@
          |Contract ByteVec() {
          |  fn test() -> () {
          |    let vec>>: ByteVec<< = b` `
          |  }
          |}
          |""".stripMargin
      )
      hints should have size 1
      hints.head.typeDef.source.parsed.fileURI should not be hints.head.parsed.fileURI
    }

    "preserve the contract target for a same-named contract value" in {
      val hints = inlayHints(
        """
          |@@
          |Contract ByteVec() {
          |  fn test() -> () {
          |    let vec>>: ByteVec<< = ByteVec(#00)
          |  }
          |}
          |""".stripMargin
      )
      hints should have size 1
      hints.head.typeDef.source.parsed.fileURI shouldBe hints.head.parsed.fileURI
    }

    "keep separate hints for different variables of a colliding primitive type" in {
      val hints = inlayHints(
        """
          |@@
          |Contract Bool() {
          |  fn test() -> () {
          |    let yes>>: Bool<< = true
          |    let no>>: Bool<< = false
          |  }
          |}
          |""".stripMargin
      )
      hints should have size 2
      hints.foreach(
        hint => hint.typeDef.source.parsed.fileURI should not be hint.parsed.fileURI
      )
    }
  }

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

    "return non-empty for immutable" when {
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

    "return non-empty mutable" when {
      "the range position is at the root" in {
        inlayHints(
          """
            |@@
            |Contract Test() {
            |  fn test() -> () {
            |    let mut one>>: U256<< = 1
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
            |    let mut one>>: U256<< = 1
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
            |    let mut one>>: U256<< = 1
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
            |    let mut @@ one>>: U256<< = 1
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
