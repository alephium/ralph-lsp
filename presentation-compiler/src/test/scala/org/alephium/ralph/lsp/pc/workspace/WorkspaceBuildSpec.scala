package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode
import org.alephium.ralph.lsp.pc.workspace.build.dependency.TestDependency
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorBuildFileNotFound, ErrorInvalidBuildFileLocation, ErrorInvalidBuildSyntax}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, TestBuild}
import org.alephium.ralphc.Config
import org.scalacheck.Gen
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.build]] function.
 */
class WorkspaceBuildSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "Build function 1: build WorkspaceState" when {
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

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          forAll(TestWorkspace.genCreated()) {
            workspace =>
              val result = Workspace.build(workspace).left.value

              result shouldBe
                BuildState.BuildErrored(
                  buildURI = workspace.buildURI,
                  code = None,
                  errors = ArraySeq(ErrorBuildFileNotFound),
                  dependency = None,
                  activateWorkspace = None
                )
          }
        }

        "workspace directory exists with no build file" in {
          implicit val file: FileAccess =
            FileAccess.disk

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          forAll(TestWorkspace.genCreated()) {
            workspace =>
              // Workspace exists, but since the workspace is Created state, there is no build file
              TestWorkspace.persist(workspace)
              TestFile.exists(workspace.workspaceURI) shouldBe true

              val result = Workspace.build(workspace).left.value

              result shouldBe
                BuildState.BuildErrored(
                  buildURI = workspace.buildURI,
                  code = None,
                  errors = ArraySeq(ErrorBuildFileNotFound),
                  dependency = None,
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

          forAll(TestBuild.genBuildParsed()) {
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
                  dependency = None,
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

          forAll(TestBuild.genBuildParsed()) {
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
                Workspace.build(workspace).value

              // sort the resulting workspace state's source code
              val actualWorkspace =
                buildResult
                  .asInstanceOf[WorkspaceState.UnCompiled]
                  .copy(sourceCode = buildResult.sourceCode.sortBy(_.fileURI))

              // expect the build to be compiled
              val expectedBuild =
                BuildState.BuildCompiled(
                  buildURI = build.buildURI,
                  code = build.code,
                  dependency = actualWorkspace.build.dependency,
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
        implicit val file: FileAccess = null
        implicit val compiler: CompilerAccess = null

        // no source or build is touched since workspace is already initialised
        val expectedWorkspace: WorkspaceState.IsSourceAware =
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

  "Build function 2: building from a build file URI" when {
    /**
     * Failed test-cases
     */
    "fail" when {
      "build file is not placed in the root workspace folder" should {

        implicit val file: FileAccess = null
        implicit val compiler: CompilerAccess = null

        /** Expect the build reports invalid build file location */
        def expectInvalidBuildLocationError(buildURI: URI,
                                            workspace: WorkspaceState.Created) = {
          val buildResult =
            Workspace.build(
              buildURI = buildURI,
              code = None,
              workspace = workspace
            ).left.value

          val expectedError =
            ErrorInvalidBuildFileLocation(
              buildURI = buildURI,
              workspaceURI = workspace.workspaceURI
            )

          buildResult shouldBe
            BuildState.BuildErrored(
              buildURI = buildURI,
              code = None,
              errors = ArraySeq(expectedError),
              dependency = None,
              activateWorkspace = None
            )
        }

        "report build error" in {
          forAll(TestWorkspace.genCreated()) {
            workspace =>
              // build file is within an "inner" folder within the workspace directory
              expectInvalidBuildLocationError(
                buildURI = Paths.get(workspace.workspaceURI).resolve("inner").resolve(Build.BUILD_FILE_NAME).toUri,
                workspace = workspace
              )

              // build file is within the parent folder of the workspace directory
              expectInvalidBuildLocationError(
                buildURI = Paths.get(workspace.workspaceURI).getParent.resolve(Build.BUILD_FILE_NAME).toUri,
                workspace = workspace
              )
          }
        }
      }

      "build file is incorrectly named" should {
        implicit val file: FileAccess = null
        implicit val compiler: CompilerAccess = null

        def expectInvalidBuildFileNameError(buildURI: URI,
                                            workspace: WorkspaceState.Created) = {
          val buildResult =
            Workspace.build(
              buildURI = buildURI,
              code = None,
              workspace = workspace
            ).left.value

          buildResult shouldBe
            BuildState.BuildErrored(
              buildURI = buildURI,
              code = None,
              errors = ArraySeq(ErrorBuildFileNotFound),
              dependency = None,
              activateWorkspace = None
            )
        }

        "report build error" in {
          forAll(TestWorkspace.genCreated()) {
            workspace =>
              // file is within an "inner" folder with in the workspace
              expectInvalidBuildFileNameError(
                buildURI = Paths.get(workspace.workspaceURI).resolve("inner").resolve("blah" + Build.BUILD_FILE_NAME).toUri,
                workspace = workspace
              )

              // file is within the parent folder of the workspace
              expectInvalidBuildFileNameError(
                buildURI = Paths.get(workspace.workspaceURI).getParent.resolve("blah" + Build.BUILD_FILE_NAME).toUri,
                workspace = workspace
              )

              // file is correctly placed in the root workspace, but has invalid file name.
              expectInvalidBuildFileNameError(
                buildURI = Paths.get(workspace.workspaceURI).resolve("blah" + Build.BUILD_FILE_NAME).toUri,
                workspace = workspace
              )
          }
        }
      }

      "build file has errors" in {
        forAll(TestBuild.genBuildParsed()) {
          _build =>
            // error build file code
            val buildCode = "blah"
            // replace parsed build file with invalid code
            val build = _build.copy(code = buildCode)
            // persist the error build file to the workspace
            TestBuild.persist(build)
            // ensure the build exists
            TestFile.exists(build.buildURI) shouldBe true

            // create a workspace for the errored build-file
            val workspace =
              WorkspaceState.Created(build.workspaceURI)

            // expect same result in both cases: When the build code is not provided and provided.
            Array(
              Some(build.code), // build code provided
              None, // build code not provided
            ) map {
              buildCode =>
                implicit val file: FileAccess =
                  if (buildCode.isDefined) // if the build code is provided, no disk IO should occur, so use null.
                    null
                  else // else allow disk IO
                    FileAccess.disk

                implicit val compiler: CompilerAccess =
                  CompilerAccess.ralphc

                // invoke build
                val actualError =
                  Workspace.build(
                    buildURI = build.buildURI,
                    code = buildCode,
                    workspace = workspace
                  ).left.value

                val expectedError =
                  BuildState.BuildErrored(
                    buildURI = workspace.buildURI,
                    code = Some(build.code),
                    errors =
                      ArraySeq(
                        ErrorInvalidBuildSyntax(
                          fileURI = build.buildURI,
                          index = SourceIndex(0, 1),
                          message = """expected json value got "b""""
                        )
                      ),
                    dependency = None,
                    activateWorkspace = None
                  )

                // the workspace should contain error targeting the build-file
                actualError shouldBe expectedError
            }

            TestWorkspace delete workspace
        }
      }
    }

    "succeed" when {
      "build file is OK" in {
        forAll(TestBuild.genBuildParsed()) {
          build =>
            // persist the error build file to the workspace
            TestBuild.persist(build)
            // ensure the build exists
            TestFile.exists(build.buildURI) shouldBe true

            // create a workspace for the errored build-file
            val workspace =
              WorkspaceState.Created(build.workspaceURI)

            // expect same result in both cases: When the build code is not provided and provided.
            Array(
              Some(build.code), // build code provided
              None, // build code not provided
            ) map {
              buildCode =>
                implicit val file: FileAccess =
                  FileAccess.disk

                implicit val compiler: CompilerAccess =
                  CompilerAccess.ralphc

                // invoke build
                val actualWorkspace =
                  Workspace.build(
                    buildURI = build.buildURI,
                    code = buildCode,
                    workspace = workspace
                  ).value

                // expect an un-compiled workspace
                val expectedWorkspace =
                  WorkspaceState.UnCompiled(
                    build =
                      BuildState.BuildCompiled(
                        buildURI = workspace.buildURI,
                        code = build.code,
                        dependency =
                          Some(TestDependency.buildStd().dependency.value), // standard dependency must be defined
                        config =
                          org.alephium.ralphc.Config( // compiler configuration must use the paths from the build file
                            build.config.compilerOptions,
                            Paths.get(workspace.workspaceURI.resolve(build.config.contractPath)),
                            Paths.get(workspace.workspaceURI.resolve(build.config.artifactPath)),
                          )
                      ),
                    sourceCode = ArraySeq.empty // workspace is empty with no source code
                  )

                actualWorkspace shouldBe expectedWorkspace
            }

            TestWorkspace delete workspace
        }
      }
    }
  }

  "Build function 3: Building from a WorkspaceFile" should {
    "not access disk" when {
      "build code is provided" in {

      }
    }
  }
}