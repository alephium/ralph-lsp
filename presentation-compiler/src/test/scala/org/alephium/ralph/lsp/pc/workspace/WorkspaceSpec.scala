package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WorkspaceSpec extends AnyWordSpec with Matchers {

  "initialise" should {
    "successfully fetch all workspace source files URIs" in {
      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

    }
  }

}
