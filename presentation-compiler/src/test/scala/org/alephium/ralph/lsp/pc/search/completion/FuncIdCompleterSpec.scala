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
