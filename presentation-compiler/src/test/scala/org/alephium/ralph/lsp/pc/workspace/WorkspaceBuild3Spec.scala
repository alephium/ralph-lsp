package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorInvalidBuildSyntax
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, TestBuild}
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.net.URI
import scala.collection.immutable.ArraySeq

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
        val buildCompiled =
          TestBuild
            .genCompiled()
            .map(TestBuild.persist)
            .sample
            .get

        implicit val file: FileAccess =
          FileAccess.disk

        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // random source-code that should be carried forward even on build compilation failure.
        val sourceCode =
          ArraySeq(SourceCodeState.OnDisk(URI.create("file:///blah.ral")))

        // run the build and expect syntax error
        val actualBuildError =
          Workspace.build(
            newBuildCode = Some("blah"),
            currentBuild = buildCompiled,
            sourceCode = sourceCode
          ).value

        // expect this build error state
        val expectedBuildError =
          BuildState.BuildErrored(
            buildURI = buildCompiled.buildURI,
            code = Some("blah"), // the invalid build code is carried forward
            errors =
              ArraySeq(
                ErrorInvalidBuildSyntax( /// the syntax error
                  fileURI = buildCompiled.buildURI,
                  index = SourceIndex(0, 1),
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
  }
}
