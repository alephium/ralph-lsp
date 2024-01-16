package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorBuildFileNotFound, ErrorInvalidBuildFileLocation}
import org.alephium.ralph.lsp.FileIO
import org.alephium.ralph.lsp.GenFile.genFolderURI
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.client.FileClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.GenBuild._
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

class BuildSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    FileClientLogger

  "build" should {
    "fail" when {
      "build in not within the workspace" in {
        // build that is outside the workspace
        val outSideBuildGen =
          GenBuild.genBuildParsed()

        // build that is inside the workspace
        val insideBuildGen =
          GenBuild
            .genBuildParsed()
            .map(persist)
            .map(Build.compile(_, None)(FileAccess.disk, CompilerAccess.ralphc, clientLogger))
            .map(_.asInstanceOf[BuildState.BuildCompiled])

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
              Build.parseAndCompile(
                buildURI = outsideBuild.buildURI,
                code = buildCode,
                currentBuild = insideBuild
              ).value

            // expected error should target the created file
            val expectedError =
              ErrorInvalidBuildFileLocation(
                buildURI = outsideBuild.buildURI,
                workspaceURI = insideBuild.workspaceURI
              )

            // build error should report the error
            val expectedWorkspace =
              BuildState.BuildErrored(
                buildURI = outsideBuild.buildURI, // must not be expected build-file location.
                code = buildCode,
                errors = ArraySeq(expectedError),
                dependency = insideBuild.dependency, // compiled dependency is carried to next compilation
                activateWorkspace = None
              )

            actualWorkspace shouldBe expectedWorkspace
        }
      }

      "build is within the workspace, but in a nested folder" in {
        // generate a build and a workspace that are in different folders
        val existingBuild =
          GenBuild
            .genBuildParsed()
            .map(persist)
            .map(Build.compile(_, None)(FileAccess.disk, CompilerAccess.ralphc, clientLogger))
            .map(_.asInstanceOf[BuildState.BuildCompiled])

        val generator =
          existingBuild flatMap {
            currentBuildParsed =>
              // build file is within a nested folder
              val buildFolder =
                currentBuildParsed.workspaceURI.resolve("nested_folder/")

              // generate a build
              GenBuild.genBuildParsed(workspaceURI = buildFolder) map {
                build =>
                  (build, currentBuildParsed)
              }
          }

        forAll(generator) {
          case (build, currentBuild) =>
            // build is within a nested folder
            val buildParentFolder = Paths.get(build.buildURI).getParent
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
              Build.parseAndCompile(
                buildURI = build.buildURI,
                code = buildCode,
                currentBuild = currentBuild
              ).value

            // expected error should target the created file
            val expectedError =
              ErrorInvalidBuildFileLocation(
                buildURI = build.buildURI,
                workspaceURI = currentBuild.workspaceURI
              )

            // build error should report the expectedError
            val expectedWorkspace =
              BuildState.BuildErrored(
                buildURI = build.buildURI,
                code = buildCode,
                errors = ArraySeq(expectedError),
                dependency = currentBuild.dependency, // compiled dependency is carried to next compilation
                activateWorkspace = None
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

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          val actual =
            Build.parseAndCompile(
              buildURI = buildURI,
              currentBuild = None
            )

          val expected =
            BuildState.BuildErrored(
              buildURI = buildURI,
              code = None,
              errors = ArraySeq(ErrorBuildFileNotFound),
              dependency = None,
              activateWorkspace = None
            )

          actual shouldBe expected
      }

    }
  }

}
