package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class StdInterfaceSpec extends AnyWordSpec with Matchers {

  "stdInterfaces" should {
    "be defined" in {
      //Will fail if web3 wasn't download correctly
      StdInterface.stdInterfaces(Paths.get("")).size > 0 shouldBe true
    }

    "respect `std/` structure" in {
      StdInterface.stdInterfaces(Paths.get("")).foreach { case (interface, _)=>
        interface.toString.contains("std/") shouldBe true
      }
    }

    "have some code" in {
      StdInterface.stdInterfaces(Paths.get("")).foreach { case (_, code)=>
        code.isEmpty shouldBe false
      }
    }
  }
}
