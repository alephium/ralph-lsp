package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.GenWorkspace._
import org.alephium.ralph.lsp.GenCommon._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.build]] function.
 */
class WorkspaceInitialiseSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  "initialise" when {
    "all files are successfully read" should {
      "start workspace in un-compiled state" in {
        forAll(genBuildCompiled(), Gen.listOf(genFileURI())) {
          case (state, fileURIs) =>
            implicit val compiler: CompilerAccess =
              mock[CompilerAccess]

            // expect the compiler to get a request to fetch files
            // from the configured contract path.
            (compiler.getSourceFiles _)
              .expects(state.build.contractURI)
              .returns(Right(fileURIs)) // return files successfully fetched
              .once() // called only once

            // Initialise a workspace for the config
            val actualWorkspace =
              Workspace.build(state)

            // All files are started in OnDisk state.
            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                build = state.build,
                sourceCode = fileURIs.map(SourceCodeState.OnDisk).to(ArraySeq)
              )

            actualWorkspace.value shouldBe expectedWorkspace
        }
      }
    }

    "failed" should {
      "report the error" in {
        forAll(genBuildCompiled(), genError()) {
          case (state, error) =>
            implicit val compiler: CompilerAccess =
              mock[CompilerAccess]

            // expect the compiler to get a request to fetch files
            // from the configured contract path.
            (compiler.getSourceFiles _)
              .expects(state.build.contractURI)
              .returns(Left(error)) // return an error
              .once() // called only once

            // Initialise a workspace for the config
            val actualWorkspace =
              Workspace.build(state)

            // No initialisation occurs and a failure is returned.
            actualWorkspace.left.value shouldBe error
        }
      }
    }
  }
}
