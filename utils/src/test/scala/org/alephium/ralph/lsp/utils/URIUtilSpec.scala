// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import scala.util.Try

class URIUtilSpec extends AnyWordSpec with Matchers {

  "URIUtil" should {
    "build and clean uri" when {
      "Windows uri contains escaped characters" in {
        if (scala.util.Properties.isWin) {
          val uri      = "file:///c%3A/Users/user/test.ral"
          val expected = "file:///c:/Users/user/test.ral"

          URIUtil.uri(uri) shouldBe new URI(expected)
        }
      }
    }

    "dropRight" should {
      "fail" when {
        "drop count is greater than the number of segments" in {
          val exception = Try(URIUtil.dropRight(URIUtil.uri("file:///a/b"), 3)).failed.get
          exception shouldBe a[IllegalArgumentException]
          exception.getMessage shouldBe "requirement failed: Drop count of 3 is not less than segment count of 2"
        }

        "drop count is equal to than the number of segments" in {
          val exception = Try(URIUtil.dropRight(URIUtil.uri("file:///a/b"), 2)).failed.get
          exception shouldBe a[IllegalArgumentException]
          exception.getMessage shouldBe "requirement failed: Drop count of 2 is not less than segment count of 2"
        }
      }

      "pass" when {
        "folders are dropped" in {
          URIUtil.dropRight(URIUtil.uri("file:///a/b/c"), 2) shouldBe URIUtil.uri("file:///a")
        }

        "file is dropped" in {
          URIUtil.dropRight(URIUtil.uri("file:///a/b/c/d.txt"), 2) shouldBe URIUtil.uri("file:///a/b")
        }
      }
    }
  }

}
