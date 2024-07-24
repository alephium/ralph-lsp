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

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorInvalidBuildSyntax
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, TestBuild}
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.util.Random

/**
 * Test cases for [[Workspace.buildSynchronised]] function.
 */
class WorkspaceBuildSynchronisedSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "Building from source-code" should {
    "fail" when {
      "new build code contains invalid syntax" in {
        // initially there exists a valid build file.
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val buildCompiled =
          TestBuild
            .genCompiledOK()
            .map(TestBuild.persist)
            .sample
            .get

        // random source-code that should be carried forward even on build compilation failure.
        val sourceCode =
          ArraySeq(SourceCodeState.OnDisk(Paths.get("blah.ral").toUri))

        // run the build and expect syntax error
        val actualBuildError =
          Workspace.buildSynchronised(
            newBuildCode = Some("blah"),
            currentBuild = buildCompiled,
            sourceCode = sourceCode
          )

        // expect this build error state
        val expectedBuildError =
          BuildState.Errored(
            buildURI = buildCompiled.buildURI,
            codeOption = Some("blah"), // the invalid build code is carried forward
            errors =                   // the syntax error
              ArraySeq(
                ErrorInvalidBuildSyntax(
                  fileURI = buildCompiled.buildURI,
                  index = SourceIndex(0, 1, Some(buildCompiled.buildURI)),
                  message = """expected json value got "b""""
                )
              ),
            dependencies = buildCompiled.dependencies, // dependency is carried forward
            activateWorkspace =                        // new workspace is activated with input source-code
              Some(
                WorkspaceState.UnCompiled(
                  build = buildCompiled,
                  sourceCode = sourceCode
                )
              )
          )

        actualBuildError.left.value shouldBe expectedBuildError
      }
    }

    "succeed" when {
      "build is unchanged" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // random source-code that should be carried forward even on build compilation failure.
        forAll(TestBuild.genCompiledWithSourceCodeInAndOut()) {
          case (buildCompiled, workspaceSourceCode, outsideSourceCode) =>
            val allSourceCode =
              Random.shuffle(workspaceSourceCode ++ outsideSourceCode)

            // run the build with the new build code as the current compiled build
            val actualWorkspace =
              Workspace
                .buildSynchronised(
                  newBuildCode = Some(buildCompiled.code), // new build code is the same as existing compiled build.
                  currentBuild = buildCompiled,
                  sourceCode = allSourceCode.to(ArraySeq) // build with all source-code
                )
                .value

            // sort the source-files
            val actualSortedWorkspace =
              actualWorkspace.copy(sourceCode = actualWorkspace.sourceCode.sortBy(_.fileURI))

            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                build = buildCompiled,
                // expect the source-code to only include the workspace's source-code.
                // outsideSourceCode are filtered out.
                sourceCode = workspaceSourceCode.to(ArraySeq)
              )

            // sort the source-files
            val expectedSortedWorkspace =
              expectedWorkspace.copy(sourceCode = expectedWorkspace.sourceCode.sortBy(_.fileURI))

            actualSortedWorkspace shouldBe expectedSortedWorkspace

            // clear generated files
            TestWorkspace delete WorkspaceState.Created(buildCompiled.workspaceURI)
        }
      }
    }
  }

}
