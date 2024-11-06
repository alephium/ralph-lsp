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

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.parse]] function.
 */
class WorkspaceSearcherFindParsedSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "return None" when {
    "file does not belong to the contract URI folder" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      forAll(TestBuild.genCompiledWithSourceCode()) {
        case (build, _sourceCode) =>
          val sourceCode =
            _sourceCode.to(ArraySeq)

          // create a workspace for the build file
          val workspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = sourceCode
            )

          // file is not in the workspace
          WorkspaceSearcher.findParsed(
            fileURI = Paths.get("blah.ral").toUri,
            workspace = workspace
          ) shouldBe None

          // not a .ral file
          WorkspaceSearcher.findParsed(
            fileURI = build.buildURI,
            workspace = workspace
          ) shouldBe None

          // file is in the root workspace directory and not in contract-uri
          WorkspaceSearcher.findParsed(
            fileURI = build.workspaceURI.resolve("blah.ral"),
            workspace = workspace
          ) shouldBe None

          // file is in the artifact directory
          build.artifactURI foreach {
            artifactURI =>
              WorkspaceSearcher.findParsed(
                fileURI = artifactURI.resolve("blah.ral"),
                workspace = workspace
              ) shouldBe None
          }
      }
    }
  }

  "pass" when {
    "belongs to the contract URI folder" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      forAll(TestBuild.genCompiledWithSourceCode(minSourceCount = 1)) {
        case (build, _sourceCode) =>
          val sourceCode =
            _sourceCode.to(ArraySeq)

          // create a workspace for the build file
          val unCompiledWorkspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = sourceCode
            )

          // workspace is successfully compiled
          val compiledWorkspace =
            Workspace
              .parseAndCompile(unCompiledWorkspace)
              .asInstanceOf[WorkspaceState.Compiled]

          // find each source code
          sourceCode foreach {
            sourceCode =>
              val parsedSource =
                WorkspaceSearcher
                  .findParsed(
                    fileURI = sourceCode.fileURI,
                    workspace = compiledWorkspace
                  )
                  .value

              parsedSource.value.fileURI shouldBe sourceCode.fileURI
          }
      }
    }
  }

}
