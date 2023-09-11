package org.alephium.ralph.lsp.pc.workspace

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import GenWorkspace._
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.config.GenCommon
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory

class WorkspaceSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  "initialise" should {
    "start workspace in un-compiled state with all files OnDisk" in {
      forAll(genWorkspaceConfig(), Gen.listOf(GenCommon.genFile())) {
        case (config, ralphFiles) =>
          implicit val compiler = mock[CompilerAccess]

          (compiler.getSourceFiles _)
            .expects(config.contractURI)
            .returns(Right(ralphFiles))

          Workspace.initialise(config)
      }
    }
  }

}
