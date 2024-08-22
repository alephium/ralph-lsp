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

package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.TestFile.genFolderURI
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild._
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorBuildFileNotFound, ErrorInvalidBuildFileLocation}
import org.scalacheck.Gen
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

class BuildSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "build" should {
    "fail" when {
      "build in not within the workspace" in {
        // build that is outside the workspace
        val outSideBuildGen =
          TestBuild.genParsed()

        // build that is inside the workspace
        val insideBuildGen =
          TestBuild
            .genParsed()
            .map(persist)
            .map(Build.compile(_, None, ArraySeq.empty)(FileAccess.disk, CompilerAccess.ralphc, clientLogger))
            .map(_.asInstanceOf[BuildState.Compiled])

        forAll(outSideBuildGen, insideBuildGen) {
          case (outsideBuild, insideBuild) =>
            // no file-io should occur
            implicit val file: FileAccess =
              null

            implicit val compiler: CompilerAccess =
              null

            // build code is optional
            val buildCode =
              Gen.option(outsideBuild.code).sample.get

            // invoke build
            val actualWorkspace =
              Build
                .parseAndCompile(
                  buildURI = outsideBuild.buildURI,
                  code = buildCode,
                  currentBuild = insideBuild,
                  dependencyDownloaders = ArraySeq.empty
                )
                .value

            // expected error should target the created file
            val expectedError =
              ErrorInvalidBuildFileLocation(
                buildURI = outsideBuild.buildURI,
                workspaceURI = insideBuild.workspaceURI
              )

            // build error should report the error
            val expectedWorkspace =
              BuildState.Errored(
                buildURI = outsideBuild.buildURI, // must not be expected build-file location.
                codeOption = buildCode,
                errors = ArraySeq(expectedError),
                dependencies = insideBuild.dependencies, // compiled dependency is carried to next compilation
                activateWorkspace = None
              )

            actualWorkspace shouldBe expectedWorkspace
        }
      }

      "build is within the workspace, but in a nested folder" in {
        // generate a build and a workspace that are in different folders
        val existingBuild =
          TestBuild
            .genParsed()
            .map(persist)
            .map(Build.compile(_, None, ArraySeq.empty)(FileAccess.disk, CompilerAccess.ralphc, clientLogger))
            .map(_.asInstanceOf[BuildState.Compiled])

        val generator =
          existingBuild flatMap {
            currentBuildParsed =>
              // build file is within a nested folder
              val buildFolder =
                currentBuildParsed.workspaceURI.resolve("nested_folder/")

              // generate a build
              TestBuild.genParsed(workspaceURI = buildFolder) map {
                build =>
                  (build, currentBuildParsed)
              }
          }

        forAll(generator) {
          case (build, currentBuild) =>
            // build is within a nested folder
            val buildParentFolder     = Paths.get(build.buildURI).getParent.getParent
            val workspaceNestedFolder = Paths.get(currentBuild.workspaceURI.resolve("nested_folder"))
            buildParentFolder shouldBe workspaceNestedFolder

            // Optional build file: Regardless, build should not get accessed from disk.
            val buildCode =
              Gen.option(build.code).sample.get

            // no file-io should occur
            implicit val file: FileAccess =
              null

            implicit val compiler: CompilerAccess =
              null

            // invoke build
            val actualWorkspace =
              Build
                .parseAndCompile(
                  buildURI = build.buildURI,
                  code = buildCode,
                  currentBuild = currentBuild,
                  dependencyDownloaders = ArraySeq.empty
                )
                .value

            // expected error should target the created file
            val expectedError =
              ErrorInvalidBuildFileLocation(
                buildURI = build.buildURI,
                workspaceURI = currentBuild.workspaceURI
              )

            // build error should report the expectedError
            val expectedWorkspace =
              BuildState.Errored(
                buildURI = build.buildURI,
                codeOption = buildCode,
                errors = ArraySeq(expectedError),
                dependencies = currentBuild.dependencies, // compiled dependency is carried to next compilation
                activateWorkspace = None
              )

            actualWorkspace shouldBe expectedWorkspace
        }
      }
    }
  }

  "parseAndCompile" should {
    "report missing build file" ignore {
      forAll(genFolderURI()) {
        workspaceDir =>
          val buildURI =
            TestFile
              .createDirectories(Build.toBuildDir(workspaceDir)) // build directory exists
              .resolve(Build.FILE_NAME)                          // build file does not exist
              .toUri

          implicit val file: FileAccess =
            FileAccess.disk

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          val actual =
            Build.parseAndCompile(
              buildURI = buildURI,
              currentBuild = None,
              dependencyDownloaders = ArraySeq.empty
            )

          val expected =
            BuildState.Errored(
              buildURI = buildURI,
              codeOption = None,
              errors = ArraySeq(ErrorBuildFileNotFound(buildURI)),
              dependencies = ArraySeq.empty,
              activateWorkspace = None
            )

          actual shouldBe expected
      }

    }
  }

}
