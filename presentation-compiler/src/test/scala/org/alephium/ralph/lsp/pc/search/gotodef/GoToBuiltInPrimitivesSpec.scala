// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToBuiltInPrimitivesSpec extends AnyWordSpec with Matchers {

  "jump to primitive" when {
    "Bool" in {
      goToBuiltInSoft(
        code = """
            |Abstract Contract Test(bool: Boo@@l) {}
            |""".stripMargin,
        expected = Some("Abstract Contract >>Bool<<")
      )
    }

    "U256" in {
      goToBuiltInSoft(
        code = """
            |Contract Test() {
            |  fn function(int: U25@@6) -> () { }
            |}
            |""".stripMargin,
        expected = Some("Abstract Contract >>U256<<")
      )
    }

    "I256" in {
      goToBuiltInSoft(
        code = """
            |Contract Test() {
            |  fn function(int: I25@@6) -> () { }
            |}
            |""".stripMargin,
        expected = Some("Abstract Contract >>I256<<")
      )
    }

    "ByteVec" in {
      goToBuiltInSoft(
        code = """
            |Contract Test() {
            |  fn function(int: ByteVe@@c) -> () { }
            |}
            |""".stripMargin,
        expected = Some("Abstract Contract >>ByteVec<<")
      )
    }

    "Address" in {
      goToBuiltInSoft(
        code = """
            |Abstract Contract Test(bool: Addres@@s) {}
            |""".stripMargin,
        expected = Some("Abstract Contract >>Address<<")
      )
    }
  }

}
