package org.alephium.ralph.lsp.pc.sourcecode

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StdInterfaceSpec extends AnyWordSpec with Matchers {

  "stdInterfaces" should {
    "be defined" in {
      //Will fail if web3 wasn't download correctly
      StdInterface.stdInterfaces.size > 0 shouldBe true
    }

    "respect `std/` structure" in {
      StdInterface.stdInterfaces.foreach { case (interface, _)=>
        interface.startsWith("std/") shouldBe true
      }
    }

    "have some code" in {
      StdInterface.stdInterfaces.foreach { case (_, code)=>
        code.isEmpty shouldBe false
      }
    }
  }
}
