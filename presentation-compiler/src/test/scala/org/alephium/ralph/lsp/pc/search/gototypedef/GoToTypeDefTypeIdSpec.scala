// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gototypedef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToTypeDefTypeIdSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "contract does not exist" when {
      "the contract's constructor is invoked" in {
        goToTypeDef(
          """
            |Contract Main() {
            |
            |  fn main(nftCollectionId: ByteVec) -> () {
            |    MyContr@@act(nftCollectionId).function()
            |  }
            |
            |}
            |""".stripMargin
        )
      }
    }
  }

  "contract exists" when {
    "the contract's constructor is invoked" in {
      goToTypeDef(
        """
          |Contract >>MyContract<<() {
          |  fn function() -> () {}
          |}
          |
          |Contract Main() {
          |
          |  fn main(nftCollectionId: ByteVec) -> () {
          |    MyContr@@act(nftCollectionId).function()
          |  }
          |  
          |}
          |""".stripMargin
      )
    }
  }

}
