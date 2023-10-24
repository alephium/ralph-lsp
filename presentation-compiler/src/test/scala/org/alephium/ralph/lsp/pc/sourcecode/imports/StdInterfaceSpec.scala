package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StdInterfaceSpec extends AnyWordSpec with Matchers {

  "stdInterfaces" should {
    val stdInterfaces = StdInterface.buildStdInterfaces.right.get
    "be defined" in {
      //Will fail if web3 wasn't download correctly
      stdInterfaces.size > 0 shouldBe true
    }

    "respect `std/` structure" in {
      stdInterfaces.foreach { case (interface, _)=>
        interface.startsWith("std/") shouldBe true
      }
    }

    "have some code" in {
      stdInterfaces.foreach { case (_, code)=>
        code.isEmpty shouldBe false
      }
    }
  }
}
