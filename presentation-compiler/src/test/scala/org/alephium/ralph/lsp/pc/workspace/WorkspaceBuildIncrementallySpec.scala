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
import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorBuildFileNotFound, ErrorInvalidBuildSyntax}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, TestBuild}
import org.scalacheck.Gen
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.build]] function.
 */
class WorkspaceBuildIncrementallySpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "build WorkspaceState" when {

    /**
     * TEST CASES: When current state is [[WorkspaceState.Created]]
     */
    "current WorkspaceState is Created" should {

      /**
       * FAIL TEST CASES
       */
      "fail" when {
        "workspace directory does not exist" ignore {
          implicit val file: FileAccess =
            FileAccess.disk

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          forAll(TestWorkspace.genCreated()) {
            workspace =>
              val result = Workspace.build(None, workspace).left.value

              result shouldBe
                BuildState.Errored(
                  buildURI = workspace.buildURI,
                  codeOption = None,
                  errors = ArraySeq(ErrorBuildFileNotFound(workspace.buildURI)),
                  tsState = None,
                  dependencies = ArraySeq.empty,
                  activateWorkspace = None
                )
          }
        }

        "workspace directory exists with no build file" ignore {
          implicit val file: FileAccess =
            FileAccess.disk

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          forAll(TestWorkspace.genCreated()) {
            workspace =>
              // Workspace exists, but since the workspace is Created state, there is no build file
              TestWorkspace.persist(workspace)
              TestFile.exists(workspace.workspaceURI) shouldBe true

              val result = Workspace.build(None, workspace).left.value

              result shouldBe
                BuildState.Errored(
                  buildURI = workspace.buildURI,
                  codeOption = None,
                  errors = ArraySeq(ErrorBuildFileNotFound(workspace.buildURI)),
                  tsState = None,
                  dependencies = ArraySeq.empty,
                  activateWorkspace = None
                )

              TestWorkspace delete workspace
          }
        }

        "workspace directory exists with error build file" in {
          implicit val file: FileAccess =
            FileAccess.disk

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          forAll(TestBuild.genParsed()) {
            build =>
              // error build file code
              val buildCode = "blah"
              // replace parsed build file with invalid code
              val errorBuild = build.copy(code = buildCode)
              // persist the error build file to the workspace
              TestBuild.persist(errorBuild)
              // ensure the build exists
              TestFile.exists(errorBuild.buildURI) shouldBe true

              // create a workspace for the errored build-file
              val workspace =
                WorkspaceState.Created(errorBuild.workspaceURI)

              // invoke build
              val result =
                Workspace.build(None, workspace).left.value

              // the workspace should contain error targeting the build-file
              result shouldBe
                BuildState.Errored(
                  buildURI = workspace.buildURI,
                  codeOption = Some(buildCode),
                  errors = ArraySeq(
                    ErrorInvalidBuildSyntax(
                      fileURI = build.buildURI,
                      index = SourceIndex(0, 1, Some(build.buildURI)),
                      message = """expected json value got "b""""
                    )
                  ),
                  tsState = None,
                  dependencies = ArraySeq.empty,
                  activateWorkspace = None
                )

              TestWorkspace delete workspace
          }
        }
      }

      /**
       * SUCCESS TEST CASES
       */
      "succeed" when {
        "all workspaces source files are successfully accessed" in {
          implicit val file: FileAccess =
            FileAccess.disk

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          forAll(TestBuild.genParsed()) {
            build =>
              // persist the build file to the workspace
              TestBuild.persist(build)
              // ensure the build exists
              TestFile.exists(build.buildURI) shouldBe true

              // generate randomly nested source-code within the build's contractPath
              val onDiskCode =
                Gen
                  .listOf(TestSourceCode.genOnDiskForBuild(build))
                  .sample
                  .get

              // persist generated source-code
              TestSourceCode.persistAll(onDiskCode)

              // create initial workspace
              val workspace =
                WorkspaceState.Created(build.workspaceURI)

              // invoke initialise on the created workspace
              val buildResult =
                Workspace.build(None, workspace).value

              // sort the resulting workspace state's source code
              val actualWorkspace =
                buildResult
                  .asInstanceOf[WorkspaceState.UnCompiled]
                  .copy(sourceCode = buildResult.sourceCode.sortBy(_.fileURI))

              // expect the build to be compiled
              val expectedBuild =
                Build
                  .compile(
                    parsed = build,
                    currentBuild = None
                  )
                  .asInstanceOf[BuildState.Compiled]

              // expect the workspace to be in un-compiled state, containing all source-code
              val expectedWorkspace =
                WorkspaceState.UnCompiled(
                  build = expectedBuild,
                  sourceCode = onDiskCode.sortBy(_.fileURI).to(ArraySeq)
                )

              // assert workspace
              actualWorkspace shouldBe expectedWorkspace

              TestWorkspace delete workspace
          }
        }
      }
    }

    /**
     * TEST CASES: When current state is [[WorkspaceState.IsSourceAware]]
     */
    "current workspace state is SourceAware" should {
      "always return the current workspace" in {
        // no file access should occur since workspace is already initialised
        implicit val file: FileAccess         = null
        implicit val compiler: CompilerAccess = null

        // no source or build is touched since workspace is already initialised
        val expectedWorkspace: WorkspaceState.IsSourceAware =
          WorkspaceState.UnCompiled(
            build = null,
            sourceCode = null
          )

        val actualWorkspace =
          Workspace.build(None, expectedWorkspace).value

        actualWorkspace shouldBe expectedWorkspace
      }
    }

  }

}
