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
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq

class SourceCodeSearcherCollectIsParsedSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val logger: ClientLogger     = TestClientLogger

  "return empty" when {
    "input is empty" in {
      SourceCodeSearcher.collectIsParsed(ArraySeq.empty) shouldBe empty
    }
  }

  "collect parsed source code even if it contains compiler errors" in {
    val onDisk =
      TestSourceCode.genOnDisk().sample.get

    val unCompiled =
      TestSourceCode.genUnCompiled().sample.get

    val strictParserError =
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

    val syntaxError =
      TestSourceCode
        .genParsedErr(
          """
          |Abstract Contract AbstractContract() { 
          |
          |contract MyContract( extends AbstractContract() {
          |  f function1() -> () {}
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
      ArraySeq(onDisk, unCompiled, strictParserError, goodCodeParsed, syntaxError, errorCompilation)

    val result =
      SourceCodeSearcher.collectIsParsed(allCode)

    result should contain only (strictParserError, goodCodeParsed, syntaxError, errorCompilation.parsed)

    TestSourceCode deleteAllIfExists allCode
  }

}
