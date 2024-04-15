// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI

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
  }

}
