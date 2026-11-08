// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import java.net.URI

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToBuiltInEventsSpec extends AnyWordSpec with Matchers {

  "native Debug" should {
    "resolve an emitted message" in {
      goToDefBuiltIn(
        code = "Contract Test() { fn test() -> () { emit De@@bug(`hello`) } }",
        expected = Some("event >>Debug<<(message: ByteVec)")
      )
    }

    "resolve while the message is incomplete" in {
      goToDefBuiltIn(
        code = "Contract Test() { fn test() -> () { emit De@@bug( } }",
        expected = Some("event >>Debug<<(message: ByteVec)")
      )
    }

    "keep user declarations rejected by the compiler" in {
      CompilerAccess
        .ralphc
        .parseStrict(
          URI.create("file:///test.ral"),
          "Contract Test() { event Debug(message: ByteVec) }"
        )
        .isLeft shouldBe true
    }

    "not appear as an ordinary function" in {
      goToDefBuiltIn(
        code = "Contract Test() { fn test() -> () { De@@bug() } }",
        expected = None
      )
    }
  }

}
