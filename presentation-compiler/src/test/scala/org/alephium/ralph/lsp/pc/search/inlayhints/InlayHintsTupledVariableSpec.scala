// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.inlayhints

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InlayHintsTupledVariableSpec extends AnyWordSpec with Matchers {

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
          |
          |    bool = true
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
          |
          |    one = 1
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
