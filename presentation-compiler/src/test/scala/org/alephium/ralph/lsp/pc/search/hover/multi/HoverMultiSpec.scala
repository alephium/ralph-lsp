// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover.multi

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.search.TestMultiCodeProvider._
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext

class HoverMultiSpec extends AnyWordSpec with Matchers {

  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val fileAccess: FileAccess   = FileAccess.disk
  implicit val logger: ClientLogger     = TestClientLogger
  implicit val ec: ExecutionContext     = ExecutionContext.Implicits.global

  "return only one hover info" in {
    val result = hoverMulti(
      workspaces = ArraySeq(
        """
            |Contract HoverTest() {
            |
            |  pub fn function() -> () {
            |
            |    let varA = true
            |    let varB = v@@arA
            |  }
            |}
            |""".stripMargin
      )
    )

    result.map(_.content.toCode()) shouldBe ArraySeq("let varA: Bool")
  }

}
