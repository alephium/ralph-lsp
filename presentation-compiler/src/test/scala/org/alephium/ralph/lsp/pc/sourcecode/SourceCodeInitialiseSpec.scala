// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.TestFile._
import org.alephium.ralph.lsp.access.compiler.message.error.TestError.genError
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.TestWorkspace
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SourceCodeInitialiseSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "initialise" should {
    "succeed" when {
      "listing fileURIs on disk passes" in {
        forAll(TestWorkspace.genCreatedWithSourceCode(persist = true)) {
          case (workspace, sourceCode) =>
            implicit val file: FileAccess =
              FileAccess.disk

            val actual =
              SourceCode
                .initialise(workspace.workspaceURI)
                .value

            actual should contain theSameElementsAs sourceCode

            // delete workspace
            TestWorkspace.delete(workspace)
        }
      }
    }

    "fail" when {
      "listing fileURIs on disk errors" in {
        forAll(genFolderURI(), genError()) {
          case (folder, error) =>
            // use mock to inject IO failure
            implicit val file: FileAccess =
              mock[FileAccess]

            (file.list _)
              .expects(folder)
              .returns(Left(error)) // inject error

            // expect error
            val actual =
              SourceCode
                .initialise(folder)
                .left
                .value

            actual shouldBe error
        }
      }
    }
  }

}
