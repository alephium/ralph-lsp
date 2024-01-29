package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode
import org.alephium.ralph.lsp.pc.workspace.build.dependency.TestDependency
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorBuildFileNotFound, ErrorInvalidBuildFileLocation, ErrorInvalidBuildSyntax}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, TestBuild}
import org.alephium.ralphc.Config
import org.scalacheck.Gen
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.build(Option[WorkspaceFile], WorkspaceState)]] function.
 */
class WorkspaceBuild3Spec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "Build function 3: Building from a WorkspaceFile" should {
    "not access disk" when {
      "build code is provided" in {

      }
    }
  }
}
