// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class FuncIdCompleterSpec extends AnyWordSpec with Matchers {

  "return non-empty" when {
    "requested for functions on a local interface" in {
      val suggestion =
        suggest {
          """
            |Interface Object {
            |
            |    pub fn deposit() -> Bool
            |    pub fn withdraw() -> ByteVec
            |    pub fn transfer() -> U256
            |
            |  }
            |
            |Contract Test(object: Object) {
            |
            |  fn function(collectionId: ByteVec) -> () {
            |    let call = Object(collectionId).@@t()
            |  }
            |
            |}
            |""".stripMargin
        }

      val completion =
        suggestion.flatMap(_.toCompletion())

      val expected =
        List(
          Completion.Method(
            label = "deposit() -> Bool",
            insert = "deposit()",
            detail = ""
          ),
          Completion.Method(
            label = "withdraw() -> ByteVec",
            insert = "withdraw()",
            detail = ""
          ),
          Completion.Method(
            label = "transfer() -> U256",
            insert = "transfer()",
            detail = ""
          )
        )

      completion shouldBe expected
    }
  }

}
