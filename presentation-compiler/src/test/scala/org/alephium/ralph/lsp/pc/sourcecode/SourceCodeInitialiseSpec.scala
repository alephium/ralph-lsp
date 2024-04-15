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
