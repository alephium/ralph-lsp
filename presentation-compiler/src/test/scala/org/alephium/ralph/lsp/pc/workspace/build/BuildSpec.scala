package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.{GenWorkspace, Workspace}
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorBuildFileNotFound, ErrorInvalidBuildFileLocation}
import org.alephium.ralph.lsp.FileIO
import org.alephium.ralph.lsp.GenCommon.genFolderURI
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

class BuildSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "build" should {
    "fail" when {
      "build in not within the workspace" in {
        // no file-io should occur
        implicit val file: FileAccess =
          null

        // generate a build and a workspace that in different folders
        forAll(GenBuild.genBuildParsed(), GenWorkspace.genCreated()) {
          case (outsideBuild, workspace) =>
            // build code is optional
            val buildCode =
              Gen.option(outsideBuild.code).sample.get

            // invoke build
            val actualWorkspace =
              Workspace.build(
                buildURI = outsideBuild.buildURI,
                code = buildCode,
                state = workspace
              ).value

            // expected error should target the created file
            val expectedError =
              ErrorInvalidBuildFileLocation(
                buildURI = outsideBuild.buildURI,
                workspaceURI = workspace.workspaceURI
              )

            // build error should report the error
            val expectedWorkspace =
              BuildState.BuildErrored(
                buildURI = outsideBuild.buildURI, // must not be expected build-file location.
                code = buildCode,
                errors = ArraySeq(expectedError)
              )

            actualWorkspace shouldBe expectedWorkspace
        }
      }

      "build is within the workspace, but in a nested folder" in {
        // generate a build and a workspace that are in different folders
        val generator =
          GenWorkspace.genCreated() flatMap {
            workspace =>
              // build file is within a nested folder
              val buildFolder =
                workspace.workspaceURI.resolve("nested_folder/")

              // generate a build
              GenBuild.genBuildParsed(workspaceURI = buildFolder) map {
                build =>
                  (build, workspace)
              }
          }

        forAll(generator) {
          case (build, workspace) =>
            // build is within a nested folder
            val buildParentFolder = Paths.get(build.buildURI).getParent
            val workspaceNestedFolder = Paths.get(workspace.workspaceURI.resolve("nested_folder"))
            buildParentFolder shouldBe workspaceNestedFolder

            // Optional build file: Regardless, build should not get accessed from disk.
            val buildCode =
              Gen.option(build.code).sample.get

            // no file-io should occur
            implicit val file: FileAccess =
              null

            // invoke build
            val actualWorkspace =
              Workspace.build(
                buildURI = build.buildURI,
                code = buildCode,
                state = workspace
              ).value

            // expected error should target the created file
            val expectedError =
              ErrorInvalidBuildFileLocation(
                buildURI = build.buildURI,
                workspaceURI = workspace.workspaceURI
              )

            // build error should report the expectedError
            val expectedWorkspace =
              BuildState.BuildErrored(
                buildURI = build.buildURI,
                code = buildCode,
                errors = ArraySeq(expectedError)
              )

            actualWorkspace shouldBe expectedWorkspace
        }
      }
    }
  }

  "parseAndCompile" should {
    "report missing build file" in {
      forAll(genFolderURI()) {
        workspaceDir =>
          val buildURI =
            FileIO
              .createDirectories(workspaceDir)
              .resolve(Build.BUILD_FILE_NAME)
              .toUri

          implicit val file: FileAccess =
            FileAccess.disk

          val actual =
            Build.parseAndCompile(buildURI)

          val expected =
            BuildState.BuildErrored(
              buildURI = buildURI,
              code = None,
              errors = ArraySeq(ErrorBuildFileNotFound)
            )

          actual shouldBe expected
      }

    }
  }

}
