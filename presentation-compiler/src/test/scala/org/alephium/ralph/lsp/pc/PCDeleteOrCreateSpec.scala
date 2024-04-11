package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{TestSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorBuildFileNotFound
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, TestBuild}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, TestWorkspace, WorkspaceFileEvent}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

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

        forAll(TestBuild.genCompiledOK()) {
          build =>
            // delete the build file
            TestBuild delete build
            // also create a deleted event
            val event =
              WorkspaceFileEvent.Deleted(build.buildURI)

            // initial/current workspace
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
                workspace = workspace,
                buildErrors = Some(
                  BuildState.BuildErrored(
                    buildURI = build.buildURI,
                    codeOption = None,                                         // because workspace is in created state
                    errors = ArraySeq(ErrorBuildFileNotFound(build.buildURI)), // the error is reported
                    dependencies = ArraySeq.empty,                             // because workspace is in created state
                    activateWorkspace = None
                  )
                )
              )

            actualPCState shouldBe expectedPCState

            TestWorkspace delete workspace
        }
      }

      "current workspace is UnCompiled" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        forAll(TestBuild.genCompiledWithSourceCodeInAndOut()) {
          case (build, sourceCodeIn, sourceCodeOut) =>
            /**
             * FAIL SCENARIO
             */

            // delete the build file
            TestBuild delete build
            // also create a deleted event
            val event =
              WorkspaceFileEvent.Deleted(build.buildURI)

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

            // invoke build event
            val actualPCState =
              PC.deleteOrCreate(
                events = ArraySeq(event),
                pcState = currentPCState
              )

            // expect PC state should maintain existing workspace (include )
            val expectedPCState =
              PCState(
                workspace = workspace,
                buildErrors = Some(
                  BuildState.BuildErrored(
                    buildURI = build.buildURI,
                    codeOption = None,                                         // no code is stored because the build is deleted
                    errors = ArraySeq(ErrorBuildFileNotFound(build.buildURI)), // the error is reported
                    dependencies = build.dependencies,                         // dependency from previous build is carried
                    activateWorkspace = Some(workspace)                        // the same workspace with inside-code and outside-code is stored.
                  )
                )
              )

            actualPCState shouldBe expectedPCState

            /**
             * PASS SCENARIO: For the same test data execute pass scenario (Just piggy back off this test).
             */
            info("PASS SCENARIO: When build is OK")
            // also test pass behaviour when build is persisted
            TestBuild persist build

            // invoke the same event
            val actualPCStateOK =
              PC.deleteOrCreate(
                events = ArraySeq(event),
                pcState = currentPCState
              )

            // no build errors this time because build file is on-disk
            actualPCStateOK.buildErrors shouldBe None
            // workspace is compiled
            val compiledWorkspace = actualPCStateOK.workspace.asInstanceOf[WorkspaceState.Compiled]
            // final workspace should contain ONLY the inside source-code, outside source-code is removed
            compiledWorkspace.sourceCode.map(_.fileURI) should contain theSameElementsAs sourceCodeIn.map(_.fileURI)

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
