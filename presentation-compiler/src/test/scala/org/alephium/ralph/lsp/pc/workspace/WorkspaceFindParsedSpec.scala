package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.parse]] function.
 */
class WorkspaceFindParsedSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "return None" when {
    "file does not belong to the contract URI folder" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      forAll(TestBuild.genCompiledWithSourceCode()) {
        case (build, _sourceCode) =>
          val sourceCode =
            _sourceCode.to(ArraySeq)

          // create a workspace for the build file
          val workspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = sourceCode
            )

          // file is not in the workspace
          Workspace.findParsed(
            fileURI = Paths.get("blah.ral").toUri,
            workspace = workspace
          ) shouldBe None

          // not a .ral file
          Workspace.findParsed(
            fileURI = build.buildURI,
            workspace = workspace
          ) shouldBe None

          // file is in the root workspace directory and not in contract-uri
          Workspace.findParsed(
            fileURI = build.workspaceURI.resolve("blah.ral"),
            workspace = workspace
          ) shouldBe None

          // file is in the artifact directory
          Workspace.findParsed(
            fileURI = build.artifactURI.resolve("blah.ral"),
            workspace = workspace
          ) shouldBe None
      }
    }
  }

  "pass" when {
    "belongs to the contract URI folder" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      forAll(TestBuild.genCompiledWithSourceCode(minSourceCount = 1)) {
        case (build, _sourceCode) =>
          val sourceCode =
            _sourceCode.to(ArraySeq)

          // create a workspace for the build file
          val unCompiledWorkspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = sourceCode
            )

          // workspace is successfully compiled
          val compiledWorkspace =
            Workspace
              .parseAndCompile(unCompiledWorkspace)
              .asInstanceOf[WorkspaceState.Compiled]

          // find each source code
          sourceCode foreach {
            sourceCode =>
              val parsedSource =
                Workspace
                  .findParsed(
                    fileURI = sourceCode.fileURI,
                    workspace = compiledWorkspace
                  )
                  .value

              parsedSource.value.fileURI shouldBe sourceCode.fileURI
          }
      }
    }
  }
}
