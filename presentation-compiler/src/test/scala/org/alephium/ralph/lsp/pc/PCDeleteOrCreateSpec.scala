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

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
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
 * Test cases for [[PC.deleteOrCreate]] function.
 */
class PCDeleteOrCreateSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

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
                buildErrors = None
              )

            // invoke delete
            val actualPCState =
              PC.deleteOrCreate(
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
                      dependencyPath = Dependency.defaultPath().value, // Default dependency build path i.e. .ralph-lsp is used
                      config = RalphcConfigState.Compiled(
                        isArtifactsPathDefinedInBuild = false, // the default build config is used above, so artifactsPath is not defined
                        config = org                           // compiled build file has full paths defined
                          .alephium
                          .ralphc
                          .Config(
                            // compiled build file contains configurations from the default build coming from node
                            compilerOptions = RalphcConfigState.Parsed.default.compilerOptions,
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
                buildErrors = None // there are no build errors
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
                buildErrors = None
              )

            // invoke the same event
            val actualPCStateOK =
              PC.deleteOrCreate(
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
                      compilerOptions = RalphcConfigState.Parsed.default.compilerOptions,
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
    "source-file is deleted" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      forAll(TestBuild.genCompiledWithSourceCodeInAndOut(minSourceCount = 1)) {
        case (build, sourceCodeIn, sourceCodeOut) =>
          val allCode =
            Random.shuffle(sourceCodeIn ++ sourceCodeOut)

          // randomly select a source-file to delete
          val sourceToDelete =
            Random.shuffle(sourceCodeIn).head

          // create a deleted event
          val event =
            WorkspaceFileEvent.Deleted(sourceToDelete.fileURI)

          // physically delete the source-file from disk
          TestSourceCode delete sourceToDelete

          // create initial workspace state with allCode (inside and out source-code)
          val workspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = allCode.to(ArraySeq)
            )

          // current PCState contains no errors
          val currentPCState =
            PCState(
              workspace = workspace,
              buildErrors = None
            )

          // invoke the event
          val actualPCState =
            PC.deleteOrCreate(
              events = ArraySeq(event),
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

          // clear test data
          TestWorkspace delete workspace
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
              buildErrors = None
            )

          // Add the all code to the workspace
          val compiledPCState =
            PC.deleteOrCreate(
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
            PC.deleteOrCreate(
              events = ArraySeq(WorkspaceFileEvent.Deleted(extension.fileURI)),
              pcState = compiledPCState
            )

          // The workspace is not compiling anymore
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
            PC.deleteOrCreate(
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
