package org.alephium.ralph.lsp.pc.workspace

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import GenWorkspace._

class WorkspaceSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "initialise" should {
    "start workspace in un-compiled state with all files OnDisk" in {
      forAll(genWorkspace()) {
        workspace =>
          ???
      }
    }
  }

}
