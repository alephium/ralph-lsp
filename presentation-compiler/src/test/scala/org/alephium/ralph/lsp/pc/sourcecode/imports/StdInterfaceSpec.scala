package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._

import java.nio.file.Paths

class StdInterfaceSpec extends AnyWordSpec with Matchers {

  implicit val logger: TestClientLogger.type =
    TestClientLogger

  "stdInterfaces" should {
    val stdInterfaces =
      StdInterface.stdInterfaces(
        dependencyPath = Paths.get("my_workspace"),
        errorIndex = SourceIndex.zero
      ).value

    "be defined" in {
      //Will fail if web3 wasn't download correctly
      stdInterfaces.nonEmpty shouldBe true
    }

    "respect `std/` structure" in {
      stdInterfaces foreach {
        source =>
          source.fileURI.toString.contains("std/") shouldBe true
      }
    }

    "have some code" in {
      stdInterfaces foreach {
        source =>
          source.code.isEmpty shouldBe false
      }
    }
  }
}
