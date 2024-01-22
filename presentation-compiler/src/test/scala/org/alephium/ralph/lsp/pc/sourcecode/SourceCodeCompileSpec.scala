package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.TestFile._
import org.alephium.ralph.lsp.access.compiler.message.error.TestError.genError
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.pc.client.FileClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.TestWorkspace
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{Dependency, DependencyDownloader, TestDependency}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, RalphcConfig}
import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.{TestCommon, TestFile}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.net.URI
import scala.collection.immutable.ArraySeq

class SourceCodeCompileSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "compile" should {
    "return empty compiled source-code" when {
      "source-code and dependency are empty" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val result =
          SourceCode.compile(
            sourceCode = ArraySeq.empty,
            dependency = None,
            compilerOptions = CompilerOptions.Default
          ).value

        result shouldBe empty
      }

      "only source-code are empty" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val dependencyBuild =
          TestDependency.buildStd()

        val result =
          SourceCode.compile(
            sourceCode = ArraySeq.empty,
            dependency = dependencyBuild.dependency.map(_.sourceCode),
            compilerOptions = CompilerOptions.Default
          ).value

        result shouldBe empty
      }
    }
  }
}
