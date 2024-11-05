// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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
