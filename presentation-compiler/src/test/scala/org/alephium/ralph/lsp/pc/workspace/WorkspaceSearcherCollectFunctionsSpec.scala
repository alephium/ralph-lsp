// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WorkspaceSearcherCollectFunctionsSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val logger: ClientLogger     = TestClientLogger

  "collect all functions within a parsed workspace" in {
    val workspace =
      TestWorkspace
        .genParsedOK(
          """
            |import "std/nft_interface"
            |
            |Abstract Contract Parent2() {
            |  fn fromParent2() -> () {}
            |}
            |
            |Abstract Contract Parent1() {
            |  fn fromParent1() -> () {}
            |}
            |
            |Contract Child() extends Parent1() {
            |  fn main() -> () {}
            |}
            |""".stripMargin
        )
        .sample
        .value

    // execute the function
    val actual =
      WorkspaceSearcher
        .collectFunctions(workspace)
        .map(_.ast.id.name)
        .toList
        .sorted

    val expected =
      List(
        // function implemented in parent2
        "fromParent2",
        // function implemented in parent1
        "fromParent1",
        // main function
        "main",
        // only the functions from imported files are suggested
        "getTokenUri",
        "getCollectionIndex"
      ).sorted

    actual shouldBe expected

    TestWorkspace delete workspace
  }

}
