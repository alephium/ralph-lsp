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
import org.scalatest.OptionValues.convertOptionToValuable

import scala.collection.immutable.ArraySeq

class SourceCodeSearcherCollectInheritedParentsForAllSpec extends AnyWordSpec with Matchers {

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
        result.map(_.tree.ast.name)

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
        result.map(_.tree.ast.name)

      val expectedNames =
        Array("Parent1", "Parent2", "Parent3")

      actualNames should contain theSameElementsAs expectedNames
    }
  }

  "soft-parser" should {
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

        SourceCodeSearcher.collectInheritedParentsForAllSoft(
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
          SourceCodeSearcher.collectInheritedParentsForAllTreesSoft(
            sourceCode = parsed,
            workspace = ArraySeq(parsed)
          )

        result should have size 3

        val actualNames =
          result.map(_.part.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text)

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
          SourceCodeSearcher.collectInheritedParentsForAllSoft(
            sourceCode = ArraySeq(parsed1, parsed2),
            workspace = ArraySeq(parsed1, parsed2)
          )

        val actualNames =
          result.map(_.part.asInstanceOf[SoftAST.Template].identifier.toOption.value.code.text)

        val expectedNames =
          Array("Parent1", "Parent2", "Parent3")

        actualNames should contain theSameElementsAs expectedNames
      }
    }
  }

}
