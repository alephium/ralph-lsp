// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.message

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.{range, SourceIndexExtension}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SourceIndexExtraSpec extends AnyWordSpec with Matchers {

  "containsSoft" when {
    "child index is on the left" in {
      //     5   -   10
      // 4  -  6
      range(5, 10) containsSoft range(4, 4) shouldBe false
      range(5, 10) containsSoft range(4, 5) shouldBe false
      range(5, 10) containsSoft range(4, 6) shouldBe false
    }

    "child index is overlapping" in {
      //     5   -   10
      //     5   -   10
      range(5, 10) containsSoft range(5, 5) shouldBe true
      range(5, 10) containsSoft range(5, 6) shouldBe true
      range(5, 10) containsSoft range(7, 9) shouldBe true
      range(5, 10) containsSoft range(10, 10) shouldBe true
      range(5, 10) containsSoft range(5, 10) shouldBe true
    }

    "child index is on the right" in {
      //     5   -   10
      //           8  - 11
      range(5, 10) containsSoft range(8, 11) shouldBe false
      range(5, 10) containsSoft range(10, 11) shouldBe false
      range(5, 10) containsSoft range(11, 11) shouldBe false
    }

    "child index is larger than parent" in {
      //     5   -   10
      //    4    -     11
      range(5, 10) containsSoft range(4, 11) shouldBe false
    }

  }

}
