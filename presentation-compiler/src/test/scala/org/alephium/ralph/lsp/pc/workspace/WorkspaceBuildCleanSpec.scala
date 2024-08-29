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

import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.alephium.ralph.lsp.pc.workspace.build.typescript.TestTSBuildFile
import org.scalacheck.Gen
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.util.Random

/**
 * Test cases for [[Workspace.buildClean]] function.
 */
class WorkspaceBuildCleanSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "clean build on ralph.json" should {
    "not access disk" when {
      "JSON code is provided" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val build =
          TestBuild
            .genCompiledOK()
            .sample
            .get

        val workspace =
          WorkspaceState.Created(build.workspaceURI)

        // Test that a workspace is initialised as un-compiled.
        def doTest(workspaceFile: WorkspaceFile) = {
          // execute build
          val actualWorkspace =
            Workspace.buildClean(
              code = Some(workspaceFile),
              workspace = workspace
            )

          val expectedWorkspace =
            WorkspaceState.UnCompiled(
              build = build,
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
    }

    "access disk" when {
      "JSON code is not provided" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val build =
          TestBuild
            .genCompiledOK()
            .sample
            .get

        val workspace =
          WorkspaceState.Created(build.workspaceURI)

        // test with a workspace-file that is either none or not a build file.
        // The end result should be the same for both.
        val workspaceFile =
          Array(
            Some(
              WorkspaceFile(
                fileURI = TestFile.genFileURI().sample.get,           // not a ralph.json URI
                text = Gen.option(TestCode.genGoodOrBad()).sample.get // with or without code
              )
            ),
            None
          )

        workspaceFile foreach {
          workspaceFile =>
            // execute build.
            val actualWorkspace =
              Workspace.buildClean(
                code = workspaceFile,
                workspace = workspace
              )

            // workspace is successfully initialised.
            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                build = build,
                sourceCode = ArraySeq.empty // workspace is empty with no source code
              )

            actualWorkspace.value shouldBe expectedWorkspace
        }

        TestWorkspace delete workspace
      }
    }
  }

  "clean build on `alephium.config.json`" should {
    "return an UnCompiled workspace" when {
      "`alephium.config.json` does not exist" in {
        implicit val file: FileAccess         = FileAccess.disk
        implicit val compiler: CompilerAccess = CompilerAccess.ralphc

        val workspace =
          TestWorkspace
            .genCreated()
            .sample
            .value

        TestWorkspace persist workspace
        // create the default `contractPath` directory
        TestFile createDirectories Paths.get(workspace.workspaceURI).resolve(RalphcConfigState.Parsed.default.contractPath)
        // no `ralph.json` file exists
        TestFile.exists(workspace.buildURI) shouldBe false

        val actualWorkspace =
          Workspace.buildClean(
            // URI for `alephium.config.ts` is supplied, but the code is not provided, nor does the file exist on disk,
            // reading from disk should still return a successfully built workspace (no failure occurs).
            code = Some(WorkspaceFile(workspace.tsBuildURI, None)),
            workspace = workspace
          )

        actualWorkspace.value.sourceCode shouldBe empty
        // `ralph.json` is generated
        TestFile.exists(workspace.buildURI) shouldBe true
        // `alephium.config.ts` is not a generated file
        TestFile.exists(workspace.tsBuildURI) shouldBe false

        TestWorkspace delete workspace
      }

      "`alephium.config.json` exists" in {
        implicit val file: FileAccess         = FileAccess.disk
        implicit val compiler: CompilerAccess = CompilerAccess.ralphc

        val workspace =
          TestWorkspace
            .genCreated()
            .sample
            .value

        TestWorkspace persist workspace
        // this time, the directory "source_dir" from `alephium.config.ts` is used,
        // instead of the generated default "contracts"
        TestFile createDirectories Paths.get(workspace.workspaceURI).resolve("source_dir")
        // generate and persist `alephium.config.ts` with a `sourceDir` set
        val tsConfig       = TestTSBuildFile.genTSConfig.sample.value.copy(sourceDir = Some("source_dir"), artifactDir = None)
        val tsConfigString = TestTSBuildFile.genTSBuildFile(tsConfig).sample.value
        TestFile.write(workspace.tsBuildURI, tsConfigString)

        // the result is the same in both cases:
        //   - When `alephium.config.ts` code is provided
        //   - When `alephium.config.ts` code is not provided
        Random.shuffle(List(Some(tsConfigString), None)) map {
          code =>
            val actualWorkspace =
              Workspace.buildClean(
                code = Some(WorkspaceFile(workspace.tsBuildURI, code)),
                workspace = workspace
              )

            actualWorkspace.value.build.parsed.config.contractPath shouldBe "source_dir"
        }

        TestWorkspace delete workspace
      }

      "`ralph.json` exists with errors" in {
        // Test that `ralph.json` may contain errors on boot-up,
        // which may get resolved after processing `alephium.config.ts`,
        // so `alephium.config.ts` should ALWAYS be processed even with an errored `ralph.json`.
        implicit val file: FileAccess         = FileAccess.disk
        implicit val compiler: CompilerAccess = CompilerAccess.ralphc

        val workspace =
          TestWorkspace
            .genCreated()
            .sample
            .value

        TestWorkspace persist workspace
        // blah is not a valid `ralph.json` code,
        // but `alephium.config.ts` contains valid code, which should fix `ralph.json`
        TestFile.write(workspace.buildURI, "blah")
        TestFile.readAll(workspace.buildURI) shouldBe "blah"
        TestFile createDirectories Paths.get(workspace.workspaceURI).resolve("source_dir")
        // generate and persist `alephium.config.ts` with a `sourceDir` set
        val tsConfig       = TestTSBuildFile.genTSConfig.sample.value.copy(sourceDir = Some("source_dir"), artifactDir = None)
        val tsConfigString = TestTSBuildFile.genTSBuildFile(tsConfig).sample.value
        TestFile.write(workspace.tsBuildURI, tsConfigString)

        // the result is the same in both cases:
        //   - When `alephium.config.ts` code is provided
        //   - When `alephium.config.ts` code is not provided
        Random.shuffle(List(Some(tsConfigString), None)) map {
          code =>
            val actualWorkspace =
              Workspace.buildClean(
                code = Some(WorkspaceFile(workspace.tsBuildURI, code)),
                workspace = workspace
              )

            actualWorkspace.value.build.parsed.config.contractPath shouldBe "source_dir"
        }

        // The `ralph.json` on-disk is parseable/has valid syntax and contractPath is updated
        val ralph_json        = TestFile.readAll(workspace.buildURI)
        val ralph_json_parsed = RalphcConfig.parse(workspace.buildURI, ralph_json).value
        ralph_json_parsed.contractPath shouldBe "source_dir"

        TestWorkspace delete workspace
      }
    }
  }

}
