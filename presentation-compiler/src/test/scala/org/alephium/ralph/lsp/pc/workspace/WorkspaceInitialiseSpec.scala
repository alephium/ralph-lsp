package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.sourcecode.GenSourceCode
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, GenBuild}
import org.alephium.ralph.lsp.FileIO
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorBuildFileNotFound, ErrorInvalidBuildSyntax}
import org.alephium.ralphc.Config
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.build]] function.
 */
class WorkspaceInitialiseSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "initialise" when {
    /**
     * TEST CASES: When current state is [[WorkspaceState.Created]]
     */
    "current WorkspaceState is Created" should {
      /**
       * FAIL TEST CASES
       */
      "fail" when {
        "workspace directory does not exist" in {
          implicit val file: FileAccess =
            FileAccess.disk

          forAll(GenWorkspace.genCreated()) {
            workspace =>
              val result = Workspace.build(workspace).left.value

              result shouldBe
                BuildState.BuildErrored(
                  buildURI = workspace.buildURI,
                  code = None,
                  errors = ArraySeq(ErrorBuildFileNotFound),
                  activateWorkspace = None
                )
          }
        }

        "workspace directory exists with no build file" in {
          implicit val file: FileAccess =
            FileAccess.disk

          forAll(GenWorkspace.genCreated()) {
            workspace =>
              // Workspace exists, but since the workspace is Created state, there is no build file
              GenWorkspace.persist(workspace)
              FileIO.exists(workspace.workspaceURI) shouldBe true

              val result = Workspace.build(workspace).left.value

              result shouldBe
                BuildState.BuildErrored(
                  buildURI = workspace.buildURI,
                  code = None,
                  errors = ArraySeq(ErrorBuildFileNotFound),
                  activateWorkspace = None
                )
          }
        }

        "workspace directory exists with error build file" in {
          implicit val file: FileAccess =
            FileAccess.disk

          forAll(GenBuild.genBuildParsed()) {
            build =>
              // error build file code
              val buildCode = "blah"
              // replace parsed build file with invalid code
              val errorBuild = build.copy(code = buildCode)
              // persist the error build file to the workspace
              GenBuild.persist(errorBuild)
              // ensure the build exists
              FileIO.exists(errorBuild.buildURI) shouldBe true

              // generate a workspace for the errored build-file
              val workspace =
                GenWorkspace
                  .genCreated(errorBuild.workspaceURI)
                  .sample
                  .get

              // invoke initialise workspace
              val result =
                Workspace.build(workspace).left.value

              // the workspace should contain error targeting the build-file
              result shouldBe
                BuildState.BuildErrored(
                  buildURI = workspace.buildURI,
                  code = Some(buildCode),
                  errors =
                    ArraySeq(
                      ErrorInvalidBuildSyntax(
                        fileURI = build.buildURI,
                        index = SourceIndex(0, 1),
                        message = """expected json value got "b""""
                      )
                    ),
                  activateWorkspace = None
                )
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

          forAll(GenBuild.genBuildParsed()) {
            build =>
              // persist the build file to the workspace
              GenBuild.persist(build)
              // ensure the build exists
              FileIO.exists(build.buildURI) shouldBe true

              // generate randomly nested source-code within the build's contractPath
              val onDiskCode =
                Gen
                  .listOf(GenSourceCode.genOnDiskForBuild(build))
                  .sample
                  .get

              // persist generated source-code
              GenSourceCode.persistAll(onDiskCode)

              // create initial workspace
              val workspace =
                WorkspaceState.Created(build.workspaceURI)

              // invoke initialise on the created workspace
              val result =
                Workspace.build(workspace).value

              // sort the resulting workspace state's source code
              val actualWorkspace =
                result
                  .asInstanceOf[WorkspaceState.UnCompiled]
                  .copy(sourceCode = result.sourceCode.sortBy(_.fileURI))

              // expect the build to be compiled
              val expectedBuild =
                BuildState.BuildCompiled(
                  buildURI = build.buildURI,
                  code = build.code,
                  config =
                    Config(
                      compilerOptions = build.config.compilerOptions,
                      contractPath = Paths.get(build.buildURI.resolve(build.config.contractPath)),
                      artifactPath = Paths.get(build.buildURI.resolve(build.config.artifactPath))
                    )
                )

              // expect the workspace to be in un-compiled state, containing all source-code
              val expectedWorkspace =
                WorkspaceState.UnCompiled(
                  build = expectedBuild,
                  sourceCode = onDiskCode.sortBy(_.fileURI).to(ArraySeq)
                )

              // assert workspace
              actualWorkspace shouldBe expectedWorkspace
          }
        }
      }
    }

    /**
     * TEST CASES: When current state is [[WorkspaceState.SourceAware]]
     */
    "current workspace state is SourceAware" should {
      "always return the current workspace" in {
        // no file access should occur since workspace is already initialised
        implicit val file: FileAccess =
          null

        // no source or build is touched since workspace is already initialised
        val expectedWorkspace: WorkspaceState.SourceAware =
          WorkspaceState.UnCompiled(
            build = null,
            sourceCode = null
          )

        val actualWorkspace =
          Workspace.build(expectedWorkspace).value

        actualWorkspace shouldBe expectedWorkspace
      }
    }

  }
}
