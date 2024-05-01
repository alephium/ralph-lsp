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

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq

class SourceCodeSearcherCollectInheritedParentsForAllSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc

  "return empty" when {
    "source-code has no inheritance" in {
      val parsed =
        TestSourceCode
          .genParsedOK(
            """
              |Contract MyContract() {
              |  fn function1() -> () {}
              |}
              |""".stripMargin
          )
          .sample
          .get

      SourceCodeSearcher.collectInheritedParentsForAll(
        sourceCode = ArraySeq(parsed),
        workspace = ArraySeq.empty
      ) shouldBe empty

      TestSourceCode deleteIfExists parsed
    }
  }

  "collect unique parent implementations" when {
    "input is a single file" in {
      val parsed =
        TestSourceCode
          .genParsedOK(
            """
              |// Struct should not be included
              |struct MyStruct {bool: Bool}
              |
              |Interface Parent3 {
              |  pub fn parent() -> U256
              |}
              |
              |Abstract Contract Parent2() extends Parent2() implements Parent3 { }
              |
              |Abstract Contract Parent1() extends Parent2(), Parent2() { }
              |
              |Contract Child1() extends Parent1() implements Parent3 {
              |  fn function1() -> () {}
              |}
              |
              |Contract Child2() extends Parent1() implements Parent3 {
              |  fn function1() -> () {}
              |}
              |""".stripMargin
          )
          .sample
          .get

      val result =
        SourceCodeSearcher.collectInheritedParentsForAllTrees(
          sourceCode = parsed,
          workspace = ArraySeq(parsed)
        )

      val actualNames =
        result.map(_.tree.ast.merge.name)

      val expectedNames =
        Array("Parent1", "Parent2", "Parent3")

      actualNames should contain theSameElementsAs expectedNames
    }

    "input contains multiple file" in {
      val parsed1 =
        TestSourceCode
          .genParsedOK(
            """
              |// Struct should not be included
              |struct MyStruct {bool: Bool}
              |
              |Abstract Contract Parent2() extends Parent2() implements Parent3 { }
              |
              |Contract Child1() extends Parent1() implements Parent3 {
              |  fn function1() -> () {}
              |}
              |""".stripMargin
          )
          .sample
          .get

      val parsed2 =
        TestSourceCode
          .genParsedOK(
            """
              |Interface Parent3 {
              |  pub fn parent() -> U256
              |}
              |
              |Abstract Contract Parent1() extends Parent2(), Parent2() { }
              |
              |Contract Child2() extends Parent1() implements Parent3 {
              |  fn function1() -> () {}
              |}
              |""".stripMargin
          )
          .sample
          .get

      val result =
        SourceCodeSearcher.collectInheritedParentsForAll(
          sourceCode = ArraySeq(parsed1, parsed2),
          workspace = ArraySeq(parsed1, parsed2)
        )

      val actualNames =
        result.map(_.tree.ast.merge.name)

      val expectedNames =
        Array("Parent1", "Parent2", "Parent3")

      actualNames should contain theSameElementsAs expectedNames
    }
  }

}
