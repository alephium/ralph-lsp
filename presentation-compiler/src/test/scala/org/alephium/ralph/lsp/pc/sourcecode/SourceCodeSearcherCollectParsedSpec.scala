// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq

class SourceCodeSearcherCollectParsedSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val logger: ClientLogger     = TestClientLogger

  "return empty" when {
    "input is empty" in {
      SourceCodeSearcher.collectParsed(ArraySeq.empty) shouldBe empty
    }
  }

  "collect parsed source code even if it contains compiler errors" in {
    val onDisk =
      TestSourceCode.genOnDisk().sample.get

    val unCompiled =
      TestSourceCode.genUnCompiled().sample.get

    val errorParser =
      TestSourceCode
        .genParsed(
          """
          |Contract MyContract() {
          |  blah
          |}
          |""".stripMargin
        )
        .sample
        .get
        .asInstanceOf[SourceCodeState.ErrorParser]

    val goodCodeParsed =
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

    val goodCodeCompiled =
      TestSourceCode
        .genCompiledOK(
          """
          |Abstract Contract AbstractContract() { }
          |
          |Contract MyContract() extends AbstractContract() {
          |  fn function1() -> () {}
          |}
          |""".stripMargin
        )
        .sample
        .get

    val errorCompilation =
      TestSourceCode
        .genCompiled(
          """
            |Contract MyContract() extends DoesNotExist() {
            |  fn function1() -> () {}
            |}
            |""".stripMargin
        )
        .sample
        .get
        .asInstanceOf[SourceCodeState.ErrorCompilation]

    val allCode =
      ArraySeq(onDisk, unCompiled, errorParser, goodCodeParsed, goodCodeCompiled, errorCompilation)

    val result =
      SourceCodeSearcher.collectParsed(allCode)

    result should contain only (goodCodeParsed, goodCodeCompiled.parsed, errorCompilation.parsed)

    TestSourceCode deleteAllIfExists allCode
  }

}
