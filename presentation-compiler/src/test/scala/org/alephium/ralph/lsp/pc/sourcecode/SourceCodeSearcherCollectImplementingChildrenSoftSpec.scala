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

import scala.collection.immutable.ArraySeq
import org.scalatest.OptionValues._

/** Copy of [[SourceCodeSearcherCollectImplementingChildrenSpec]] for testing [[SoftAST]] */
class SourceCodeSearcherCollectImplementingChildrenSoftSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val logger: ClientLogger     = TestClientLogger

  "return empty" when {
    "input source-code has no inheritance" in {
      val parsed =
        TestSourceCode
          .genParsedErr(
            """
              |Interface contract( {
              |  fn function1( - () {
              |""".stripMargin
          )
          .sample
          .get

      val tree  = parsed.astSoft.fetch().value
      val parts = tree.partsNonEmpty
      parts should have size 1
      val part = parts.head
      part shouldBe a[SoftAST.Template]

      SourceCodeSearcher.collectImplementingChildren(
        source = SourceLocation.CodeSoft(part, parsed),
        allSource = ArraySeq.empty
      ) shouldBe empty

      TestSourceCode deleteIfExists parsed
    }
  }

  "collect single child implementation" when {
    def doTest(code: String) = {
      val parsed =
        TestSourceCode
          .genParsedErr(code)
          .sample
          .get

      val parsedTrees =
        SourceCodeSearcher
          .collectSourceTreesSoft(parsed)
          .to(ArraySeq)

      val softAST =
        parsed.astSoft.fetch().value

      val parts = softAST.partsNonEmpty

      // first statement is Parent()
      val parent = parts.head.asInstanceOf[SoftAST.Template]
      parent.identifier.toOption.value.code.text shouldBe "Parent"

      // second statement is Child()
      val child = parts(1).asInstanceOf[SoftAST.Template]
      child.identifier.toOption.value.code.text shouldBe "Child"

      // expect parent to be returned
      val expected =
        SourceLocation.CodeSoft(
          part = parts(1),
          parsed = parsed
        )

      val actual =
        SourceCodeSearcher.collectImplementingChildren(
          source = SourceLocation.CodeSoft(parts.head, parsed),
          allSource = parsedTrees
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
      val file1 =
        TestSourceCode
          .genParsedErr(
            """
              |Abstract Contract Parent2() extends Parent4(), Parent6() implements Parent1 { }
              |
              |// Interface is implemented
              |Interface Parent1 {
              |  pub f parent() -> U256
              |}
              |
              |// Parent3 is not extends by Child(), it should not be in the result
              |Abstract Contract Parent3() extends Parent1(), Parent1() { }
              |
              |Contract Child() extends Parent2(), Child(), Parent5() implements Parent1 {
              |  pu n parent() -> U256 {
              |    rturn 1
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
        file1.astSoft.fetch().value.partsNonEmpty

      // collect all trees from file2
      val treesFromFile2 =
        file2.astSoft.fetch().value.partsNonEmpty

      // the first statement in file2 is Parent6()
      val parent = treesFromFile2.head
      parent.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text shouldBe "Parent6"

      // expect children to be returned excluding Parent1() and Parent3()
      val expectedTreesFromFile1 =
        treesFromFile1
          .filterNot {
            tree =>
              val name = tree.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text
              name == "Parent1" || name == "Parent3"
          }
          .map {
            child =>
              SourceLocation.CodeSoft(
                part = child,
                parsed = file1 // file1 is in scope
              )
          }

      val expectedTreesFromFile2 =
        treesFromFile2
          .filterNot {
            tree =>
              val name = tree.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text
              name == "Parent6"
          }
          .map {
            child =>
              SourceLocation.CodeSoft(
                part = child,
                parsed = file2 // file2 is in scope
              )
          }

      // collect all parent trees to expect
      val expectedTrees =
        expectedTreesFromFile1 ++ expectedTreesFromFile2

      val allTrees =
        SourceCodeSearcher.collectSourceTreesSoft(ArraySeq(file1, file2))

      // actual trees returned
      val actual =
        SourceCodeSearcher.collectImplementingChildren(
          source = SourceLocation.CodeSoft(parent, file2),
          allSource = allTrees
        )

      actual should contain theSameElementsAs expectedTrees

      // Double check: Also assert the names of the parents.
      val parentNames = actual.map(_.part.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text)
      // Note: Parent3 and Child are not included.
      parentNames should contain only ("Parent4", "Parent2", "Parent5", "Child")

      TestSourceCode deleteAllIfExists Array(file1, file2)
    }
  }

}
