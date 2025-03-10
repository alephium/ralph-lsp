// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.file.FileAccess
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq

class SourceCodeSearcherCollectImplementingChildrenSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc

  "return empty" when {
    "input source-code has no inheritance" in {
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
        parsed.astStrict.statements.head.asInstanceOf[Tree.Source]

      SourceCodeSearcher.collectImplementingChildren(
        source = SourceLocation.CodeStrict(tree, parsed),
        allSource = ArraySeq.empty
      ) shouldBe empty

      TestSourceCode deleteIfExists parsed
    }
  }

  "collect single child implementation" when {
    def doTest(code: String) = {
      val parsed =
        TestSourceCode
          .genParsedOK(code)
          .sample
          .get

      val parsedTrees =
        SourceCodeSearcher
          .collectSourceTrees(parsed)
          .to(ArraySeq)

      // first statement is Parent()
      val parent = parsed.astStrict.statements.head.asInstanceOf[Tree.Source]
      parent.ast.name shouldBe "Parent"

      // second statement is Child()
      val child = parsed.astStrict.statements.last.asInstanceOf[Tree.Source]
      child.ast.name shouldBe "Child"

      // expect parent to be returned
      val expected =
        SourceLocation.CodeStrict(
          tree = child,
          parsed = parsed
        )

      val actual =
        SourceCodeSearcher.collectImplementingChildren(
          source = SourceLocation.CodeStrict(parent, parsed),
          allSource = parsedTrees
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
              |Abstract Contract Parent3() extends Parent1(), Parent1() { }
              |
              |Contract Child() extends Parent2(), Child(), Parent5() implements Parent1 {
              |  pub fn parent() -> U256 {
              |    return 1
              |  }
              |}
              |""".stripMargin
          )
          .sample
          .get

      // file2 contains the Parent6() contract for which the children are collected.
      val file2 =
        TestSourceCode
          .genParsedOK(
            """
              |// Parent6's children are being collect in this test
              |Abstract Contract Parent6() extends Parent4() { }
              |
              |Abstract Contract Parent5() extends Parent4(), Parent5() { }
              |
              |Abstract Contract Parent4() extends Parent5(), Parent6(), Parent4() { }
              |""".stripMargin
          )
          .sample
          .get

      // collect all trees from file1
      val treesFromFile1 =
        file1.astStrict.statements.map(_.asInstanceOf[Tree.Source])

      // collect all trees from file2
      val treesFromFile2 =
        file2.astStrict.statements.map(_.asInstanceOf[Tree.Source])

      // the first statement in file2 is Parent6()
      val parent = treesFromFile2.head
      parent.ast.name shouldBe "Parent6"

      // expect children to be returned excluding Parent1() and Parent3()
      val expectedTreesFromFile1 =
        treesFromFile1
          .filterNot {
            tree =>
              tree.ast.name == "Parent1" || tree.ast.name == "Parent3"
          }
          .map {
            child =>
              SourceLocation.CodeStrict(
                tree = child,
                parsed = file1 // file1 is in scope
              )
          }

      val expectedTreesFromFile2 =
        treesFromFile2
          .filterNot {
            tree =>
              tree.ast.name == "Parent6"
          }
          .map {
            child =>
              SourceLocation.CodeStrict(
                tree = child,
                parsed = file2 // file2 is in scope
              )
          }

      // collect all parent trees to expect
      val expectedTrees =
        expectedTreesFromFile1 ++ expectedTreesFromFile2

      val allTrees =
        SourceCodeSearcher.collectSourceTrees(ArraySeq(file1, file2))

      // actual trees returned
      val actual =
        SourceCodeSearcher.collectImplementingChildren(
          source = SourceLocation.CodeStrict(parent, file2),
          allSource = allTrees
        )

      actual should contain theSameElementsAs expectedTrees

      // Double check: Also assert the names of the parents.
      val parentNames = actual.map(_.tree.ast.asInstanceOf[Ast.ContractWithState].name)
      // Note: Parent3 and Child are not included.
      parentNames should contain only ("Parent4", "Parent2", "Parent5", "Child")

      TestSourceCode deleteAllIfExists Array(file1, file2)
    }
  }

}
