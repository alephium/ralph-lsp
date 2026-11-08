// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.inlayhints

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InlayHintsTupledVariableSpec extends AnyWordSpec with Matchers {

  "nested tuples" should {
    "display one complete hint for a tuple among multiple return values" in {
      inlayHints(
        """
          |@@
          |Contract Test() {
          |  fn values() -> (Bool, ByteVec, ((Bool, U256))) {
          |    return (true, #00, (true, 1))
          |  }
          |  fn main() -> () {
          |    let (flag>>: Bool<<,
          |         bytes>>: ByteVec<<,
          |         tuple>>: (Bool, U256)<<) = values()
          |  }
          |}
          |""".stripMargin
      )
    }

    "preserve recursive shape and repeated leaf types" in {
      inlayHints(
        """
          |@@
          |Contract Test() {
          |  fn values() -> (Bool, ((Bool, (U256, Bool)))) {
          |    return (true, (true, (1, false)))
          |  }
          |  fn main() -> () {
          |    let (flag>>: Bool<<,
          |         tuple>>: (Bool, (U256, Bool))<<) = values()
          |  }
          |}
          |""".stripMargin
      )
    }

    "select each element when destructuring a tuple expression" in {
      inlayHints(
        """
          |@@
          |Contract Test() {
          |  fn main() -> () {
          |    let (flag>>: Bool<<,
          |         number>>: U256<<) = (true, 1)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  /**
   * **********************************
   * Test cases: Named tupled variable.
   * **********************************
   */
  "return non-empty named variables" when {
    "range is at the root" in {
      inlayHints(
        """
          |@@
          |Contract Test() {
          |  fn test() -> () {
          |    let (one>>: U256<<,
          |         bool>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is in the contract" in {
      inlayHints(
        """
          |Contract Test() {
          |  @@
          |  fn test() -> () {
          |    let (one>>: U256<<,
          |         mut bool>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is in the function" in {
      inlayHints(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    @@
          |    let (mut one>>: U256<<,
          |         bool>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is in the variable" in {
      inlayHints(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    let (@@ one>>: U256<<,
          |         bool>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is at after the first variable" in {
      inlayHints(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    // Note: One will receive no hint 
          |    let (one,
          |         @@
          |         bool>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return empty named variables" when {
    "range is at the end" in {
      inlayHints(
        """
          |Contract Test() {
          |  fn test() -> () {
          |    let (one, bool) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |@@
          |""".stripMargin
      )
    }

    "range is in the contract" in {
      inlayHints(
        """
          |Contract Test() {
          |  fn test() -> () {
          |    let (one, bool) = tuple()
          |  }
          |
          |  @@
          |  
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is in the function" in {
      inlayHints(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    let (one, bool) = tuple()
          |    @@
          |  }
          |  
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  /**
   * ************************************
   * Test cases: Unnamed tupled variable.
   * ************************************
   */
  "return non-empty unnamed variables" when {
    "range is at the root" in {
      inlayHints(
        """
          |@@
          |Contract Test() {
          |  fn test() -> () {
          |    let (_>>: U256<<,
          |         _>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is in the contract" in {
      inlayHints(
        """
          |Contract Test() {
          |  @@
          |  fn test() -> () {
          |    let (_>>: U256<<,
          |         _>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is in the function" in {
      inlayHints(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    @@
          |    let (_>>: U256<<,
          |         _>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is in the variable" in {
      inlayHints(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    let (@@ _>>: U256<<,
          |         _>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is at after the first variable" in {
      inlayHints(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    // Note: _ will receive no hint
          |    let (_,
          |         @@
          |         _>>: Bool<<) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  "return empty unnamed variables" when {
    "range is at the end" in {
      inlayHints(
        """
          |Contract Test() {
          |  fn test() -> () {
          |    let (_, _) = tuple()
          |  }
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |@@
          |""".stripMargin
      )
    }

    "range is in the contract" in {
      inlayHints(
        """
          |Contract Test() {
          |  fn test() -> () {
          |    let (_, _) = tuple()
          |  }
          |
          |  @@
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }

    "range is in the function" in {
      inlayHints(
        """
          |Contract Test() {
          |
          |  fn test() -> () {
          |    let (_, _) = tuple()
          |    @@
          |  }
          |
          |
          |  fn tuple() -> (U256, Bool) {
          |    return (1, true)
          |  }
          |}
          |""".stripMargin
      )
    }
  }

}
