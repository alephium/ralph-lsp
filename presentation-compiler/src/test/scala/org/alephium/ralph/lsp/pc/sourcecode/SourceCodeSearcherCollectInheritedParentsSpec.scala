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

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.file.FileAccess
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq

class SourceCodeSearcherCollectInheritedParentsSpec extends AnyWordSpec with Matchers {

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

      val tree =
        parsed.ast.statements.head.asInstanceOf[Tree.Source]

      SourceCodeSearcher.collectInheritedParents(
        source = SourceLocation.Code(tree, parsed),
        allSource = ArraySeq.empty
      ) shouldBe empty

      TestSourceCode deleteIfExists parsed
    }
  }

  "collect single parent implementations" when {
    def doTest(code: String) = {
      val parsed =
        TestSourceCode
          .genParsedOK(code)
          .sample
          .get

      // first statement is Parent()
      val parent = parsed.ast.statements.head.asInstanceOf[Tree.Source]
      parent.ast.name shouldBe "Parent"

      // second statement is Child()
      val child = parsed.ast.statements.last.asInstanceOf[Tree.Source]
      child.ast.name shouldBe "Child"

      // expect parent to be returned
      val expected =
        SourceLocation.Code(
          tree = parent,
          parsed = parsed
        )

      // Collect all trees in the parser source file
      val allTrees =
        SourceCodeSearcher
          .collectSourceTrees(parsed)
          .to(ArraySeq)

      val actual =
        SourceCodeSearcher.collectInheritedParents(
          source = SourceLocation.Code(child, parsed),
          allSource = allTrees
        )

      actual should contain only expected

      TestSourceCode deleteIfExists parsed
    }

    "parent is an Abstract Contract" in {
      doTest {
        """
          |Abstract Contract Parent() { }
          |
          |Contract Child() extends Parent() {
          |  fn function1() -> () {}
          |}
          |""".stripMargin
      }
    }

    "parent is an Interface" in {
      doTest {
        """
          |Interface Parent {
          |  pub fn parent() -> U256
          |}
          |
          |Contract Child() implements Parent {
          |  pub fn parent() -> U256 {
          |    return 1
          |  }
          |}
          |""".stripMargin
      }
    }
  }

  "collect deep inheritance" when {
    "it also contains cyclic and duplicate inheritance" in {
      // file1 contains the Child() contract for which the parents are collected.
      val file1 =
        TestSourceCode
          .genParsedOK(
            """
              |Abstract Contract Parent2() extends Parent4(), Parent6() implements Parent1 { }
              |
              |// Interface is implemented
              |Interface Parent1 {
              |  pub fn parent() -> U256
              |}
              |
              |// Parent3 is not extends by Child(), it should not be in the result
              |Abstract Contract Parent3() extends Parent4(), Parent4() { }
              |
              |// The child parents to collect
              |Contract Child() extends Parent2(), Child() implements Parent1 {
              |  pub fn parent() -> U256 {
              |    return 1
              |  }
              |}
              |""".stripMargin
          )
          .sample
          .get

      // file2 contains all other abstract contracts
      val file2 =
        TestSourceCode
          .genParsedOK(
            """
              |Abstract Contract Parent6() extends Parent4() { }
              |
              |Abstract Contract Parent5() extends Parent4(), Parent5() { }
              |
              |Abstract Contract Parent4() extends Parent5(), Parent6(), Parent4() { }
              |""".stripMargin
          )
          .sample
          .get

      // collect all tree from file1
      val treesFromFile1 =
        file1.ast.statements.map(_.asInstanceOf[Tree.Source])

      // collect all tree from file2
      val treesFromFile2 =
        file2.ast.statements.map(_.asInstanceOf[Tree.Source])

      // the last statement in file1 is Child()
      val child = treesFromFile1.last
      child.ast.name shouldBe "Child"

      // expect parents to be returned excluding Parent3() and Child()
      val expectedTreesFromFile1 =
        treesFromFile1
          .filterNot {
            tree =>
              tree.ast.name == "Parent3" || tree.ast.name == "Child"
          }
          .map {
            parent =>
              SourceLocation.Code(
                tree = parent,
                parsed = file1 // file1 is in scope
              )
          }

      val expectedTreesFromFile2 =
        treesFromFile2
          .map {
            parent =>
              SourceLocation.Code(
                tree = parent,
                parsed = file2 // file2 is in scope
              )
          }

      // collect all parent trees to expect
      val expectedTrees =
        expectedTreesFromFile1 ++ expectedTreesFromFile2

      // Collect all trees
      val allTrees =
        SourceCodeSearcher.collectSourceTrees(ArraySeq(file1, file2))

      // actual trees returned
      val actual =
        SourceCodeSearcher.collectInheritedParents(
          source = SourceLocation.Code(child, file1),
          allSource = allTrees
        )

      actual should contain theSameElementsAs expectedTrees

      // Double check: Also assert the names of the parents.
      val parentNames = actual.map(_.tree.ast.asInstanceOf[Ast.ContractWithState].name)
      // Note: Parent3 and Child are not included.
      parentNames should contain only ("Parent1", "Parent2", "Parent4", "Parent5", "Parent6")

      TestSourceCode deleteAllIfExists Array(file1, file2)
    }
  }

}
