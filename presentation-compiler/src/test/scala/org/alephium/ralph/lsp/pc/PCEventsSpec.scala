// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{TestSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.Dependency
import org.alephium.ralph.lsp.pc.workspace.build.{TestRalphc, BuildState, TestBuild}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, TestWorkspace, WorkspaceFileEvent}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.util.Random

/**
 * Test cases for [[PC.events]] function.
 */
class PCEventsSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "report failure" when {
    "build file is deleted" when {
      "current workspace is Created" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // set `dependenciesFolderName` to `None` so that dependencies get written to the default folder.
        val buildGenerator =
          TestBuild.genCompiledOK(
            config = TestRalphc.genRalphcParsedConfig(
              dependenciesFolderName = None,
              contractsFolderName = RalphcConfigState.Parsed.default.contractPath,
              artifactsFolderName = RalphcConfigState.Parsed.default.artifactPath
            )
          )

        forAll(buildGenerator) {
          build =>
            // delete the build and also create a deleted event
            val event =
              if (Random.nextBoolean()) {
                // delete the `build.json` file
                val buildURI = TestBuild deleteFile build
                TestFile.exists(buildURI) shouldBe false
                WorkspaceFileEvent.Deleted(buildURI)
              } else {
                // delete the `.ralph-lsp` directory containing the `build.json` file
                val dirURI = TestBuild deleteDirectory build
                TestFile.exists(dirURI) shouldBe false
                WorkspaceFileEvent.Deleted(dirURI)
              }

            val workspace =
              WorkspaceState.Created(build.workspaceURI)

            val currentPCState =
              PCState(
                workspace = workspace,
                buildErrors = None,
                tsErrors = None
              )

            // invoke delete
            val actualPCState =
              PC.events(
                events = ArraySeq(event),
                pcState = currentPCState
              )

            val expectedPCState =
              PCState(
                // expected workspace
                workspace = WorkspaceState.Compiled(
                  sourceCode = ArraySeq.empty,    // there is no source code
                  parsed = WorkspaceState.Parsed( // Workspace is successful parsed
                    build = BuildState.Compiled(
                      dependencies = build.dependencies,               // default dependencies are written
                      dependencyPath = Dependency.defaultPath().value, // Default dependency build path i.e. `.ralph-lsp` is used
                      config = RalphcConfigState.Compiled(
                        isArtifactsPathDefinedInBuild = false, // the default build config is used above, so artifactsPath is not defined
                        config = org                           // compiled build file has full paths defined
                          .alephium
                          .ralphc
                          .Config(
                            // compiled build file contains configurations from the default build coming from node
                            compilerOptions = CompilerOptions.Default,
                            contractPath = Paths.get(build.workspaceURI).resolve(RalphcConfigState.Parsed.default.contractPath),
                            artifactPath = Paths.get(build.workspaceURI).resolve(RalphcConfigState.Parsed.default.contractPath)
                          )
                      ),
                      parsed = BuildState.Parsed(
                        build.buildURI,
                        RalphcConfig.write(RalphcConfigState.Parsed.default, indent = 2), // Default build file is written,
                        RalphcConfigState.Parsed.default
                      )
                    ),
                    sourceCode = ArraySeq.empty // there is no source-code in this workspace
                  )
                ),
                buildErrors = None, // there are no build errors
                tsErrors = None
              )

            // build file exists on disk
            TestFile.exists(build.buildURI) shouldBe true

            actualPCState shouldBe expectedPCState

            // clear workspace
            TestWorkspace delete workspace
        }
      }

      "current workspace is UnCompiled" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // Generate
        val buildGenerator =
          TestBuild
            .genCompiledWithSourceCodeInAndOut(
              config = TestRalphc
                .genRalphcParsedConfig(
                  dependenciesFolderName = None,
                  contractsFolderName = RalphcConfigState.Parsed.default.contractPath,
                  artifactsFolderName = RalphcConfigState.Parsed.default.artifactPath
                )
            )

        forAll(buildGenerator) {
          case (build, sourceCodeIn, sourceCodeOut) =>
            // delete the build and also create a deleted event
            val event =
              if (Random.nextBoolean()) {
                // delete the `build.json` file
                val buildURI = TestBuild deleteFile build
                TestFile.exists(buildURI) shouldBe false
                WorkspaceFileEvent.Deleted(buildURI)
              } else {
                // delete the `.ralph-lsp` directory containing the `build.json` file
                val dirURI = TestBuild deleteDirectory build
                TestFile.exists(dirURI) shouldBe false
                WorkspaceFileEvent.Deleted(dirURI)
              }

            val workspace =
              WorkspaceState.UnCompiled(
                build = build,
                // a workspace with source-code that inside and outside the workspace
                // expected that outside source-code is still maintained (previous state is maintained) since build is errored.
                // No state change should occur, if the build is errored.
                sourceCode = Random.shuffle(sourceCodeIn ++ sourceCodeOut).to(ArraySeq)
              )

            // current PCState contains no errors
            val currentPCState =
              PCState(
                workspace = workspace,
                buildErrors = None,
                tsErrors = None
              )

            // invoke the same event
            val actualPCStateOK =
              PC.events(
                events = ArraySeq(event),
                pcState = currentPCState
              )

            // A build file is generated
            TestFile.exists(build.buildURI) shouldBe true

            // no build errors because build file is generate
            actualPCStateOK.buildErrors shouldBe None
            // workspace is compiled
            val compiledWorkspace = actualPCStateOK.workspace.asInstanceOf[WorkspaceState.Compiled]
            // final workspace should contain ONLY the inside source-code, outside source-code is removed
            compiledWorkspace.sourceCode.map(_.fileURI) should contain theSameElementsAs sourceCodeIn.map(_.fileURI)

            // Expect this default build file
            val expectedBuild =
              BuildState.Compiled(
                dependencies = build.dependencies,               // default dependencies are written
                dependencyPath = Dependency.defaultPath().value, // Default dependency build path i.e. .ralph-lsp is used
                config = RalphcConfigState.Compiled(
                  isArtifactsPathDefinedInBuild = false, // the default build config is used above, so artifactsPath is not defined
                  config = org                           // compiled build file has full paths defined
                    .alephium
                    .ralphc
                    .Config(
                      // compiled build file contains configurations from the default build coming from node
                      compilerOptions = CompilerOptions.Default,
                      contractPath = Paths.get(build.workspaceURI).resolve(RalphcConfigState.Parsed.default.contractPath),
                      artifactPath = Paths.get(build.workspaceURI).resolve(RalphcConfigState.Parsed.default.contractPath)
                    )
                ),
                BuildState.Parsed(
                  buildURI = build.buildURI,
                  code = RalphcConfig.write(RalphcConfigState.Parsed.default, indent = 2), // Default build file is written
                  config = RalphcConfigState.Parsed.default
                )
              )

            compiledWorkspace.build shouldBe expectedBuild

            // clear test data
            TestWorkspace delete workspace
        }
      }
    }
  }

  "succeed" when {
    "one source-file deleted and another is changed" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      // ensure a minimum of 2 source files are created within the workspace
      forAll(TestBuild.genCompiledWithSourceCodeInAndOut(minSourceCount = 2)) {
        case (build, sourceCodeIn, sourceCodeOut) =>
          // all source files shuffled
          val allCodeOnDisk = Random.shuffle(sourceCodeIn ++ sourceCodeOut)
          // only the workspace source files shuffled
          val shuffledSourceCodeIn = Random.shuffle(sourceCodeIn)

          // randomly select a source-file to delete
          val sourceToDelete = shuffledSourceCodeIn.head
          // randomly select a source-file to change
          val sourceToChange = shuffledSourceCodeIn.last

          // create a deleted event
          val deleteEvent = WorkspaceFileEvent.Deleted(sourceToDelete.fileURI)
          // physically delete the source-file from disk
          TestFile exists sourceToDelete.fileURI // it should exist
          TestSourceCode delete sourceToDelete   // delete it

          // create a changed event
          val changeEvent = WorkspaceFileEvent.Changed(sourceToChange.fileURI)
          // overwrite the changed source-file on disk
          val preUpdateCode = TestFile readAll sourceToChange.fileURI
          val updatedCode   = "Abstract Contract UpdateGoodCode() { }"
          preUpdateCode should not be updatedCode
          // change the existing code
          TestFile.write(sourceToChange.fileURI, updatedCode)

          // create initial workspace state with allCode (inside and out source-code)
          val workspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = allCodeOnDisk.to(ArraySeq)
            )

          // current PCState contains no errors
          val currentPCState =
            PCState(
              workspace = workspace,
              buildErrors = None,
              tsErrors = None
            )

          // invoke both the event
          val actualPCState =
            PC.events(
              events = ArraySeq(deleteEvent, changeEvent),
              pcState = currentPCState
            )

          // expect no build errors
          actualPCState.buildErrors shouldBe None
          // workspace is compiled OK
          val compiledWorkspace = actualPCState.workspace.asInstanceOf[WorkspaceState.Compiled]
          // Final workspace should contain ONLY the inside source-code.
          // The deleted file and the outside source-code should be removed.
          val expectedSourceCode = sourceCodeIn.filter(_.fileURI != sourceToDelete.fileURI)
          compiledWorkspace.sourceCode.map(_.fileURI) should contain theSameElementsAs expectedSourceCode.map(_.fileURI)
          // the Changed Event should result in the new code being saved
          val changedSourceFile = compiledWorkspace.sourceCode.find(_.fileURI == sourceToChange.fileURI).value
          changedSourceFile.parsed.code shouldBe updatedCode

          // clear test data
          TestWorkspace delete workspace
          TestSourceCode deleteAll sourceCodeOut
      }
    }

    "abstract contract is deleted then created" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      forAll(TestBuild.genExtendedContract()) {
        case (build, contract, extension, extensionCode, extensionName) =>
          val allCode = ArraySeq(contract, extension)

          // Create an empty uncompiled workspace
          val workspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = ArraySeq.empty
            )

          val initialPCState =
            PCState(
              workspace = workspace,
              buildErrors = None,
              tsErrors = None
            )

          // Add the all code to the workspace
          val compiledPCState =
            PC.events(
              events = allCode.to(ArraySeq).map {
                code =>
                  WorkspaceFileEvent.Created(code.fileURI)
              },
              pcState = initialPCState
            )

          // The workspace compile and contains both abstract contract and contract
          val compiledWorkspace = compiledPCState.workspace.asInstanceOf[WorkspaceState.Compiled]

          compiledWorkspace.sourceCode.map(_.fileURI) should contain theSameElementsAs allCode.map(_.fileURI)

          // Delete the abstract contract
          TestSourceCode delete extension
          val erroredState =
            PC.events(
              events = ArraySeq(WorkspaceFileEvent.Deleted(extension.fileURI)),
              pcState = compiledPCState
            )

          // The workspace is not compiling any more
          val erroredWorkspace = erroredState.workspace.asInstanceOf[WorkspaceState.Errored]

          // But error isn't at workspace level
          erroredWorkspace.workspaceErrors shouldBe empty

          // The error is in the contract file, as the abstract contract is missing
          erroredWorkspace.sourceCode.size shouldBe 1
          val sourceState = erroredWorkspace.sourceCode.head.asInstanceOf[SourceCodeState.ErrorCompilation]
          sourceState.fileURI shouldBe contract.fileURI
          val messages = sourceState.errors.map(_.message)
          messages.size shouldBe 1
          messages.head shouldBe s"""Contract "$extensionName" does not exist"""

          // Recreate the abstract contract
          TestSourceCode persist (extension, code = Gen.const(extensionCode))

          val recompiledPCState =
            PC.events(
              events = ArraySeq(WorkspaceFileEvent.Created(extension.fileURI)),
              pcState = erroredState
            )

          val recompiledWorkspace = recompiledPCState.workspace.asInstanceOf[WorkspaceState.Compiled]
          recompiledWorkspace.sourceCode.map(_.fileURI) should contain theSameElementsAs allCode.map(_.fileURI)

          TestWorkspace delete workspace
      }
    }
  }

}
