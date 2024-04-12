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

package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
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
