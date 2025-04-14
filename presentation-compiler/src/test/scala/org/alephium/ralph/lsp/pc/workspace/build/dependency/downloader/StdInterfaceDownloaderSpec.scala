// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class StdInterfaceDownloaderSpec extends AnyWordSpec with Matchers {

  implicit val logger: TestClientLogger.type =
    TestClientLogger

  "stdInterfaces" should {
    val stdInterfaces =
      StdInterfaceDownloader
        .download(
          dependencyPath = Paths.get("my_workspace"),
          errorIndex = SourceIndexExtra.zero(TestFile.genFolderURI().sample.value)
        )
        .value
        .sourceCode
        .map(_.asInstanceOf[SourceCodeState.UnCompiled])

    "be defined" in {
      // Will fail if web3 wasn't download correctly
      stdInterfaces.nonEmpty shouldBe true
    }

    "respect `std/` structure" in {
      stdInterfaces foreach {
        source =>
          source.fileURI.toString.contains(s"${DependencyID.Std.dirName}/") shouldBe true
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
