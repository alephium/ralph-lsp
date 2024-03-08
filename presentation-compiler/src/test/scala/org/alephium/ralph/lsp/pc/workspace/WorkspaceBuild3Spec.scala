package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorInvalidBuildSyntax
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, TestBuild}
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.util.Random

/**
 * Test cases for [[Workspace.build(Option[String], BuildState.BuildCompiled, ArraySeq[SourceCodeState])]] function.
 */
class WorkspaceBuild3Spec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "Build function 3: Building from source-code" should {
    "fail" when {
      "new build code contains invalid syntax" in {
        // initially there exists a valid build file.
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val buildCompiled =
          TestBuild
            .genCompiledOK()
            .map(TestBuild.persist)
            .sample
            .get

        // random source-code that should be carried forward even on build compilation failure.
        val sourceCode =
          ArraySeq(SourceCodeState.OnDisk(Paths.get("blah.ral").toUri))

        // run the build and expect syntax error
        val actualBuildError =
          Workspace.build(
            newBuildCode = Some("blah"),
            currentBuild = buildCompiled,
            sourceCode = sourceCode
          )

        // expect this build error state
        val expectedBuildError =
          BuildState.BuildErrored(
            buildURI = buildCompiled.buildURI,
            code = Some("blah"), // the invalid build code is carried forward
            errors =
              ArraySeq(
                ErrorInvalidBuildSyntax( /// the syntax error
                  fileURI = buildCompiled.buildURI,
                  index = SourceIndex(0, 1, Some(buildCompiled.buildURI)),
                  message = """expected json value got "b""""
                )
              ),
            dependency = buildCompiled.dependency, // dependency is carried forward
            activateWorkspace = // new workspace is activated with input source-code
              Some(
                WorkspaceState.UnCompiled(
                  build = buildCompiled,
                  sourceCode = sourceCode
                )
              )
          )

        actualBuildError.left.value shouldBe expectedBuildError
      }
    }

    "succeed" when {
      "build is unchanged" in {
        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // random source-code that should be carried forward even on build compilation failure.
        forAll(TestBuild.genCompiledWithSourceCodeInAndOut()) {
          case (buildCompiled, workspaceSourceCode, outsideSourceCode) =>
            val allSourceCode =
              Random.shuffle(workspaceSourceCode ++ outsideSourceCode)

            // run the build with the new build code as the current compiled build
            val actualWorkspace =
              Workspace.build(
                newBuildCode = Some(buildCompiled.code), //new build code is the same as existing compiled build.
                currentBuild = buildCompiled,
                sourceCode = allSourceCode.to(ArraySeq) // build with all source-code
              ).value

            // sort the source-files
            val actualSortedWorkspace =
              actualWorkspace.copy(sourceCode = actualWorkspace.sourceCode.sortBy(_.fileURI))

            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                build = buildCompiled,
                // expect the source-code to only include the workspace's source-code.
                // outsideSourceCode are filtered out.
                sourceCode = workspaceSourceCode.to(ArraySeq)
              )

            // sort the source-files
            val expectedSortedWorkspace =
              expectedWorkspace.copy(sourceCode = expectedWorkspace.sourceCode.sortBy(_.fileURI))

            actualSortedWorkspace shouldBe expectedSortedWorkspace

            // clear generated files
            TestWorkspace delete WorkspaceState.Created(buildCompiled.workspaceURI)
        }
      }
    }
  }
}
