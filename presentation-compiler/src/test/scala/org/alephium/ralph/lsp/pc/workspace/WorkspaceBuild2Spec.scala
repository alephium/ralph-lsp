package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.build(Option[WorkspaceFile], WorkspaceState)]] function.
 */
class WorkspaceBuild2Spec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "Build function 2: Building from a WorkspaceFile" should {
    "not access disk" when {
      "build code is provided" in {
        val build =
          TestBuild
            .genParsed()
            .map(TestBuild.persist)
            .sample
            .get

        val workspace =
          WorkspaceState.Created(build.workspaceURI)

        // Test that a workspace is initialised as un-compiled.
        def doTest(workspaceFile: WorkspaceFile) = {
          implicit val file: FileAccess =
            FileAccess.disk

          implicit val compiler: CompilerAccess =
            CompilerAccess.ralphc

          // execute build
          val actualWorkspace =
            Workspace.build(
              code = Some(workspaceFile),
              workspace = workspace
            )

          val expectedWorkspace =
            WorkspaceState.UnCompiled(
              build = TestBuild.toCompiled(build),
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

      "workspace state is already source-ware" in {
        // No IO should occur
        implicit val file: FileAccess =
          null

        implicit val compiler: CompilerAccess =
          null

        // a source-aware workspace
        val sourceAware: WorkspaceState.IsSourceAware =
          WorkspaceState.Compiled(
            sourceCode = ArraySeq.empty,
            parsed = null
          )

        val actualWorkspace =
          Workspace.build(
            code = None,
            workspace = sourceAware
          )

        // returns the same workspace without processing it.
        actualWorkspace.value shouldBe sourceAware
      }
    }

    "access disk" when {
      "build code is not provided" in {
        val build =
          TestBuild
            .genParsed()
            .map(TestBuild.persist)
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
                fileURI = TestFile.genFileURI().sample.get,
                text = None // build's code is None, so it will be fetch from disk.
              )
            ),
            None
          )

        workspaceFile foreach {
          workspaceFile =>
            implicit val file: FileAccess =
              FileAccess.disk

            implicit val compiler: CompilerAccess =
              CompilerAccess.ralphc

            // execute build.
            val actualWorkspace =
              Workspace.build(
                code = workspaceFile,
                workspace = workspace
              )

            // workspace is successfully initialised.
            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                build = TestBuild.toCompiled(build),
                sourceCode = ArraySeq.empty // workspace is empty with no source code
              )

            actualWorkspace.value shouldBe expectedWorkspace
        }

        TestWorkspace delete workspace
      }
    }
  }
}
