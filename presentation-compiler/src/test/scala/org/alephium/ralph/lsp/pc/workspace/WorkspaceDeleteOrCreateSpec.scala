package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState , TestSourceCode}
import org.alephium.ralph.lsp.pc.state.PCState
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorBuildFileNotFound
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, TestBuild}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck.Gen

import scala.collection.immutable.ArraySeq
import scala.util.Random

/**
 * Test cases for [[Workspace.deleteOrCreate]] function.
 */
class WorkspaceDeleteOrCreateSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

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
              Workspace.deleteOrCreate(
                events = ArraySeq(event),
                pcState = currentPCState
              )

            val expectedPCState =
              PCState(
                workspace = workspace,
                buildErrors =
                  Some(
                    BuildState.BuildErrored(
                      buildURI = build.buildURI,
                      code = None, // because workspace is in created state
                      errors = ArraySeq(ErrorBuildFileNotFound(build.buildURI)), // the error is reported
                      dependency = None, // because workspace is in created state
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
              Workspace.deleteOrCreate(
                events = ArraySeq(event),
                pcState = currentPCState
              )

            // expect PC state should maintain existing workspace (include )
            val expectedPCState =
              PCState(
                workspace = workspace,
                buildErrors =
                  Some(
                    BuildState.BuildErrored(
                      buildURI = build.buildURI,
                      code = None, // no code is stored because the build is deleted
                      errors = ArraySeq(ErrorBuildFileNotFound(build.buildURI)), // the error is reported
                      dependency = build.dependency, // dependency from previous build is carried
                      activateWorkspace = Some(workspace) // the same workspace with inside-code and outside-code is stored.
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
              Workspace.deleteOrCreate(
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
            Workspace.deleteOrCreate(
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

    "source-file is deleted then created" in {
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
            Workspace.deleteOrCreate(
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


          //recreate the source-file
          TestSourceCode persist sourceToDelete

          val createEvent =
            WorkspaceFileEvent.Created(sourceToDelete.fileURI)

          val latestPCState =
            Workspace.deleteOrCreate(
              events = ArraySeq(createEvent),
              pcState = actualPCState
            )
          latestPCState.buildErrors shouldBe None
          // workspace is compiled OK
          val latestWorkspace = latestPCState.workspace.asInstanceOf[WorkspaceState.Compiled]
          // Final workspace should contain ONLY the inside source-code.
          // The deleted file and the outside source-code should be removed.
          latestWorkspace.sourceCode.map(_.fileURI) should contain(sourceToDelete.fileURI)
          // clear test data
          TestWorkspace delete workspace
      }
    }


    "prout" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

          {
          val build = TestBuild.genCompiledOK().sample.get
        // forAll(TestBuild.genCompiledOK()) {
        //   build =>
            val abstName = "Prout"
            val abst = TestCode.genAbstract(Gen.const(abstName)).sample.get
            val contract = TestCode.genExtendedContract(extension = Gen.const(abstName)).sample.get
            val a = TestSourceCode.genOnDiskForRoot(rootURI = Gen.const(build.contractURI)).sample.get
            val c = TestSourceCode.genOnDiskForRoot(rootURI = Gen.const(build.contractURI)).sample.get

            TestSourceCode persist (a, code = Gen.const( abst))
            TestSourceCode persist (c, code = Gen.const( contract))


            println(s"${Console.RED}${Console.BOLD}*** build.workspaceURI ***${Console.RESET}${build.workspaceURI}")
            println(s"${Console.RED}${Console.BOLD}*** a ***${Console.RESET}${a}")
            println(s"${Console.RED}${Console.BOLD}*** c ***${Console.RESET}${c}")

            val workspace =
              WorkspaceState.Created(build.workspaceURI)

          // current PCState contains no errors
          val currentPCState =
            PCState(
              workspace = workspace,
              buildErrors = None
            )

          val pcState =
            Workspace.deleteOrCreate(
              events = ArraySeq(
            WorkspaceFileEvent.Created(a.fileURI),
            WorkspaceFileEvent.Created(c.fileURI)
          ),
              pcState = currentPCState
            )

          val latestWorkspace = pcState.workspace.asInstanceOf[WorkspaceState.Compiled]
          val uris = latestWorkspace.sourceCode.map(_.fileURI)
          println(s"${Console.RED}${Console.BOLD}*** uris ***${Console.RESET}${uris}")

          latestWorkspace.sourceCode.map(_.fileURI) should contain theSameElementsAs uris

          TestSourceCode delete a
          val event =
            WorkspaceFileEvent.Deleted(a.fileURI)

          val state =
            Workspace.deleteOrCreate(
              events = ArraySeq(
            WorkspaceFileEvent.Deleted(a.fileURI)
          ),
              pcState = pcState
            )

          val err=state.workspace.asInstanceOf[WorkspaceState.Errored].sourceCode.flatMap(_.asInstanceOf[SourceCodeState.ErrorSource].errors)

            err.map(_.message) should contain ("""Contract "Prout" does not exist""")

            // recreate the source-file
            TestSourceCode persist (a, code = Gen.const( abst))

          val state2 =
            Workspace.deleteOrCreate(
              events = ArraySeq(
            WorkspaceFileEvent.Created(a.fileURI),
          ),
              pcState = state
            )

          val w = state2.workspace.asInstanceOf[WorkspaceState.Compiled].sourceCode
          println(s"${Console.RED}${Console.BOLD}*** w ***${Console.RESET}${w}")
          //val u = w.sourceCode.map(_.fileURI)
          //println(s"${Console.RED}${Console.BOLD}*** uris ***${Console.RESET}${u}")

          TestWorkspace delete workspace
      }
    }
  }
}
