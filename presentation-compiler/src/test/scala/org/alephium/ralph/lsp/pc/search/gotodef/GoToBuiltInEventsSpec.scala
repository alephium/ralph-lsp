// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import java.net.URI
import java.nio.file.Paths

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.Dependency
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.BuiltInFunctionDownloader
import org.scalatest.EitherValues._

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToBuiltInEventsSpec extends AnyWordSpec with Matchers {

  "native Debug" should {
    "resolve an emitted message" in {
      goToDefBuiltIn(
        code = "Contract Test() { fn test() -> () { emit De@@bug(`hello`) } }",
        expected = Some("struct >>Debug<< { message: ByteVec }")
      )
    }

    "resolve while the message is incomplete" in {
      goToDefBuiltIn(
        code = "Contract Test() { fn test() -> () { emit De@@bug( } }",
        expected = Some("struct >>Debug<< { message: ByteVec }")
      )
    }

    "reparse persisted metadata with the unmodified strict parser" in {
      val workspace = BuiltInFunctionDownloader
        .download(
          Paths.get("native-metadata"),
          SourceIndexExtra.zero(URI.create("file:///test.ral"))
        )(TestClientLogger)
        .value
      val source = workspace
        .sourceCode
        .collectFirst {
          case source: SourceCodeState.UnCompiled if source.fileURI.toString.endsWith(Dependency.EVENT_FILE_NAME) => source
        }
        .get
      CompilerAccess.ralphc.parseStrict(source.fileURI, source.code).isRight shouldBe true
      source.code should include("struct Debug { message: ByteVec }")
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

    "not appear as an ordinary struct constructor" in {
      goToDefBuiltIn(
        code = "Contract Test() { fn test() -> () { let value = De@@bug { message: #00 } } }",
        expected = None
      )
    }

    "not appear as an ordinary function" in {
      goToDefBuiltIn(
        code = "Contract Test() { fn test() -> () { De@@bug() } }",
        expected = None
      )
    }
  }

}
