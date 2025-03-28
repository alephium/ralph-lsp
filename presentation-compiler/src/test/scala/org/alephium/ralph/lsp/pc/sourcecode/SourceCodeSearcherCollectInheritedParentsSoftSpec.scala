// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._

import scala.collection.immutable.ArraySeq

/** Copy of [[SourceCodeSearcherCollectInheritedParentsSpec]] for testing [[SoftAST]] */
class SourceCodeSearcherCollectInheritedParentsSoftSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val logger: ClientLogger     = TestClientLogger

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

      val tree  = parsed.astSoft.fetch().value
      val parts = tree.partsNonEmpty
      parts should have size 1
      val part = parts.head
      part shouldBe a[SoftAST.Template]

      SourceCodeSearcher.collectInheritedParents(
        source = SourceLocation.CodeSoft(part, parsed),
        allSource = ArraySeq.empty
      ) shouldBe empty

      TestSourceCode deleteIfExists parsed
    }
  }

  "collect single parent implementations" when {
    def doTest(code: String) = {
      val parsed =
        TestSourceCode
          .genParsedErr(code)
          .sample
          .get

      val softAST = parsed.astSoft.fetch().value
      val parts   = softAST.partsNonEmpty

      // first statement is Parent()
      val parentPart = parts.head
      val parent     = parentPart.asInstanceOf[SoftAST.Template]
      parent.identifier.toOption.value.code.text shouldBe "Parent"

      // second statement is Child()
      val childPart = parts(1)
      val child     = childPart.asInstanceOf[SoftAST.Template]
      child.identifier.toOption.value.code.text shouldBe "Child"

      // expect parent to be returned
      val expected =
        SourceLocation.CodeSoft(
          part = parentPart,
          parsed = parsed
        )

      // Collect all trees in the parser source file
      val allTrees =
        SourceCodeSearcher
          .collectSourceTreesSoft(parsed)
          .to(ArraySeq)

      val actual =
        SourceCodeSearcher.collectInheritedParents(
          source = SourceLocation.CodeSoft(childPart, parsed),
          allSource = allTrees
        )

      actual should contain only expected

      TestSourceCode deleteIfExists parsed
    }

    "parent is an Abstract Contract" in {
      doTest {
        """
          |Abstract Contract Parent( {}
          |
          |Contract Child( extends Parent( {
          |  f function1( -> () {}
          |}
          |""".stripMargin
      }
    }

    "parent is an Interface" in {
      doTest {
        """
          |Interface Parent {
          |  pub fn parent() - U256
          |}
          |
          |Contract Child( implements Parent {
          |  pub f parent( -> U256
          |    rturn 1
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
        file1.astSoft.fetch().value.partsNonEmpty

      // collect all tree from file2
      val treesFromFile2 =
        file2.astSoft.fetch().value.partsNonEmpty

      // the last statement in file1 is Child()
      val child = treesFromFile1.last
      child.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text shouldBe "Child"

      // expect parents to be returned excluding Parent3() and Child()
      val expectedTreesFromFile1 =
        treesFromFile1
          .filterNot {
            tree =>
              val name = tree.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text
              name == "Parent3" || name == "Child"
          }
          .map {
            parent =>
              SourceLocation.CodeSoft(
                part = parent,
                parsed = file1 // file1 is in scope
              )
          }

      val expectedTreesFromFile2 =
        treesFromFile2
          .map {
            parent =>
              SourceLocation.CodeSoft(
                part = parent,
                parsed = file2 // file2 is in scope
              )
          }

      // collect all parent trees to expect
      val expectedTrees =
        expectedTreesFromFile1 ++ expectedTreesFromFile2

      // Collect all trees
      val allTrees =
        SourceCodeSearcher.collectSourceTreesSoft(ArraySeq(file1, file2))

      // actual trees returned
      val actual =
        SourceCodeSearcher.collectInheritedParents(
          source = SourceLocation.CodeSoft(child, file1),
          allSource = allTrees
        )

      actual should contain theSameElementsAs expectedTrees

      // Double check: Also assert the names of the parents.
      val parentNames = actual.map(_.part.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text)
      // Note: Parent3 and Child are not included.
      parentNames should contain only ("Parent1", "Parent2", "Parent4", "Parent5", "Parent6")

      TestSourceCode deleteAllIfExists Array(file1, file2)
    }
  }

}
