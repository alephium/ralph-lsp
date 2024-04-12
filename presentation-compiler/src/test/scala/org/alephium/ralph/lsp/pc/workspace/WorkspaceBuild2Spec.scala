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

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, TestBuild}
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.build(Option[WorkspaceFile], WorkspaceState)]] function.
 */
class WorkspaceBuild2Spec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "Build function 2: Building from a WorkspaceFile" should {
    "not access disk" when {
      "build code is provided" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val build =
          TestBuild
            .genParsed()
            .map(TestBuild.persist)
            .sample
            .get

        val compiledBuild =
          Build
            .compile(
              parsed = build,
              currentBuild = None
            )
            .asInstanceOf[BuildState.BuildCompiled]

        val workspace =
          WorkspaceState.Created(build.workspaceURI)

        // Test that a workspace is initialised as un-compiled.
        def doTest(workspaceFile: WorkspaceFile) = {
          // execute build
          val actualWorkspace =
            Workspace.build(
              code = Some(workspaceFile),
              workspace = workspace
            )

          val expectedWorkspace =
            WorkspaceState.UnCompiled(
              build = compiledBuild,
              sourceCode = ArraySeq.empty // workspace is empty with no source code
            )

          actualWorkspace.value shouldBe expectedWorkspace
        }

        // Test 1: Build code is not supplied
        doTest(
          WorkspaceFile(
            fileURI = build.buildURI,
            text = None // expect IO because build code is not provided.
          )
        )

        // Test 2: Build code is supplied
        doTest {
          // Delete the build file to test that no IO occurs.
          TestFile delete build.buildURI

          WorkspaceFile(
            fileURI = build.buildURI,
            text = Some(build.code) // build's code is supplied, so no IO should occur.
          )
        }

        TestWorkspace delete workspace
      }

      "workspace state is already source-ware" in {
        // No IO should occur
        implicit val file: FileAccess =
          null

        implicit val compiler: CompilerAccess =
          null

        // a source-aware workspace
        val sourceAware: WorkspaceState.IsSourceAware =
          WorkspaceState.Compiled(
            sourceCode = ArraySeq.empty,
            parsed = null
          )

        val actualWorkspace =
          Workspace.build(
            code = None,
            workspace = sourceAware
          )

        // returns the same workspace without processing it.
        actualWorkspace.value shouldBe sourceAware
      }
    }

    "access disk" when {
      "build code is not provided" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val build =
          TestBuild
            .genParsed()
            .map(TestBuild.persist)
            .sample
            .get

        val compiledBuild =
          Build
            .compile(
              parsed = build,
              currentBuild = None
            )
            .asInstanceOf[BuildState.BuildCompiled]

        val workspace =
          WorkspaceState.Created(build.workspaceURI)

        // test with a workspace-file that is either none or not a build file.
        // The end result should be the same for both.
        val workspaceFile =
          Array(
            Some(
              WorkspaceFile(
                fileURI = TestFile.genFileURI().sample.get,
                text = None // build's code is None, so it will be fetch from disk.
              )
            ),
            None
          )

        workspaceFile foreach {
          workspaceFile =>
            // execute build.
            val actualWorkspace =
              Workspace.build(
                code = workspaceFile,
                workspace = workspace
              )

            // workspace is successfully initialised.
            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                build = compiledBuild,
                sourceCode = ArraySeq.empty // workspace is empty with no source code
              )

            actualWorkspace.value shouldBe expectedWorkspace
        }

        TestWorkspace delete workspace
      }
    }
  }

}
