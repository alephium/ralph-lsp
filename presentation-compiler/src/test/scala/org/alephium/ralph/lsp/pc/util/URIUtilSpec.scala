package org.alephium.ralph.lsp.pc.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI

class URIUtilSpec extends AnyWordSpec with Matchers {

  "URIUtil" should {
    "build and clean uri" when {
      "Windows uri contains escaped characters" in {
        if (scala.util.Properties.isWin) {
          val uri = "file:///c%3A/Users/user/test.ral"
          val expected = "file:///c:/Users/user/test.ral"

          URIUtil.uri(uri) shouldBe new URI(expected )
        }
      }
    }
  }
}
