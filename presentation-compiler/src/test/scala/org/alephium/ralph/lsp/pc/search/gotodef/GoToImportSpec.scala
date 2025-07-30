// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToImportSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "file is searched" when {
      "folder exists, file does not exist" in {
        goToDefStd(None) {
          """
            |import "std/b@@lah"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }

      "folder does not exists, file exists" in {
        goToDefStd(None) {
          """
            |import "blah/nft_inte@@rface"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }
    }

    "folder does not exist" in {
      goToDefStd(None) {
        """
          |import "bla@@h/nft_interface"
          |
          |Abstract Contract Test() { }
          |""".stripMargin
      }
    }
  }

  "file name is selected" when {
    "first character" in {
      goToDefStd(Some(">><<@std(id = #0003)")) {
        """
          |import "std/@@nft_interface"
          |
          |Abstract Contract Test() { }
          |""".stripMargin
      }
    }

    "mid character" in {
      goToDefStd(Some(">><<@std(id = #0003)")) {
        """
          |import "std/nft_in@@terface"
          |
          |Abstract Contract Test() { }
          |""".stripMargin
      }
    }

    "last character" in {
      goToDefStd(Some(">><<@std(id = #0003)")) {
        """
          |import "std/nft_interface@@"
          |
          |Abstract Contract Test() { }
          |""".stripMargin
      }
    }
  }

  "folder name is selected" when {
    val expected =
      Iterable(
        ">><<@std(id = #0001)",
        ">><<@std(enabled = false)",
        ">><<@std(id = #0002)",
        ">><<@std(id = #0003)",
        ">><<@std(id = #000201)"
      )

    "only folder name is provided (file name is missing)" when {
      "first character" in {
        goToDefStdSoft(expected) {
          """
            |import "@@std"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }

      "mid character" in {
        goToDefStdSoft(expected) {
          """
            |import "st@@d"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }

      "last character" in {
        goToDefStdSoft(expected) {
          """
            |import "std@@"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }
    }

    "file name exists" when {
      "first character" in {
        goToDefStd(expected) {
          """
            |import "@@std/nft_interface"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }

      "mid character" in {
        goToDefStd(expected) {
          """
            |import "st@@d/nft_interface"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }

      "last character" in {
        goToDefStd(expected) {
          """
            |import "std@@/nft_interface"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }
    }

    "file name does not exist" when {
      "first character" in {
        goToDefStd(expected) {
          """
            |import "@@std/blah"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }

      "mid character" in {
        goToDefStd(expected) {
          """
            |import "st@@d/blah"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }

      "last character" in {
        goToDefStd(expected) {
          """
            |import "std@@/blah"
            |
            |Abstract Contract Test() { }
            |""".stripMargin
        }
      }
    }
  }

}
