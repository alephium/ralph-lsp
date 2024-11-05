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

package org.alephium.ralph.lsp.access.compiler

import com.typesafe.scalalogging.Logger
import org.alephium.ralph.{Warning, SourceIndex, Ast}
import org.alephium.ralph.lsp.utils.log.{ClientLogger, LogMessage}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

import java.net.URI

class ExtractWarningsSpec extends AnyWordSpec with Matchers with MockFactory {

  "return empty" when {
    "warnings are empty" in {
      implicit val logger: ClientLogger =
        null // nothing should get logged

      val code =
        CompiledCodeWrapper(TestRalphc.createCompiledContract(Ast.TypeId("MyContract")))

      RalphCompilerAccess.extractWarnings(code) shouldBe empty
    }

    "the warning and Contract have no SourceIndex" in {
      val contractTypeId =
        Ast.TypeId("MyContract")

      val warning =
        Warning(message = "Wassup!", sourceIndex = None)

      val code =
        CompiledCodeWrapper(TestRalphc.createCompiledContract(contractTypeId, warning))

      implicit val logger: ClientLogger =
        mock[ClientLogger]

      (logger
        .error(_: LogMessage)(_: Logger))
        .expects(CompilerLogMessage.UnassignedWarningNoneFileInfo(warning, contractTypeId.name), *)
        .returns(())
        .once()

      RalphCompilerAccess.extractWarnings(code) shouldBe empty
    }

    "the warning and Contract have SourceIndex, but fileURI is not provided" in {
      val contractTypeId =
        Ast.TypeId("MyContract")

      val warning =
        Warning(message = "Wassup!", sourceIndex = Some(SourceIndex(1, 2, None)))

      val code =
        CompiledCodeWrapper(TestRalphc.createCompiledContract(contractTypeId, warning))

      implicit val logger: ClientLogger =
        mock[ClientLogger]

      (logger
        .error(_: LogMessage)(_: Logger))
        .expects(CompilerLogMessage.UnassignedWarningNoneFileURI(warning, contractTypeId.name), *)
        .returns(())
        .once()

      RalphCompilerAccess.extractWarnings(code) shouldBe empty
    }

  }

  "return nonempty" when {
    "Contract has SourceIndex but the warning and Contract.Ident has no SourceIndex" in {
      val contractTypeId =
        Ast.TypeId("MyContract")

      val ast =
        TestRalphc.createTestContract(contractTypeId)

      val contractSourceIndex =
        SourceIndex(1, 2, Some(URI.create("file://blah.txt")))

      ast.sourceIndex = Some(contractSourceIndex)

      val warning =
        Warning(message = "Wassup!", sourceIndex = None)

      val code =
        CompiledCodeWrapper(TestRalphc.createCompiledContract(ast, warning))

      implicit val logger: ClientLogger =
        mock[ClientLogger]

      (logger
        .error(_: LogMessage)(_: Logger))
        .expects(CompilerLogMessage.NoneIdentSourceIndex(warning, contractTypeId.name), *)
        .returns(())
        .once()

      // Warning and Ast.Contract.Ident has no SourceIndex, therefore, the SourceIndex of Ast.Contract should be used.
      val expected =
        warning.copy(sourceIndex = Some(contractSourceIndex))

      val actual =
        RalphCompilerAccess.extractWarnings(code)

      actual should contain only expected
    }

    "Warning does not have a SourceIndex, but Contract.Ident does" in {
      val typeIdSourceIndex =
        SourceIndex(1, 2, Some(URI.create("file://blah.txt")))

      val contractTypeId =
        Ast.TypeId("MyContract")

      contractTypeId.sourceIndex = Some(typeIdSourceIndex)

      val ast =
        TestRalphc.createTestContract(contractTypeId)

      ast.sourceIndex shouldBe empty

      val warning =
        Warning(message = "Wassup!", sourceIndex = None)

      val code =
        CompiledCodeWrapper(TestRalphc.createCompiledContract(ast, warning))

      implicit val logger: ClientLogger =
        null // nothing should get logged

      // Warning does not have a SourceIndex but the Contract's TypeId does
      val expected =
        warning.copy(sourceIndex = Some(typeIdSourceIndex))

      val actual =
        RalphCompilerAccess.extractWarnings(code)

      actual should contain only expected
    }

    "Warning has SourceIndex but its fileURI is None, but Contract has a fileURI" in {
      val contractTypeId =
        Ast.TypeId("MyContract")

      val ast =
        TestRalphc.createTestContract(contractTypeId)

      val astSourceIndex =
        SourceIndex(1, 2, Some(URI.create("file://blah.txt")))

      ast.sourceIndex = Some(astSourceIndex)

      val warningSourceIndex =
        SourceIndex(4, 5, None)

      val warning =
        Warning(message = "Wassup!", sourceIndex = Some(warningSourceIndex))

      val code =
        CompiledCodeWrapper(TestRalphc.createCompiledContract(ast, warning))

      implicit val logger: ClientLogger =
        null // nothing should get logged

      // Warning.SourceIndex.fileURI is None, therefore, Contract's fileURI should be used
      val expected =
        warning.copy(sourceIndex = warning.sourceIndex.map(_.copy(fileURI = Some(astSourceIndex.fileURI.value))))

      val actual =
        RalphCompilerAccess.extractWarnings(code)

      actual should contain only expected
    }

    "Warning has SourceIndex" when {
      def doTest(astSourceIndex: Option[SourceIndex]) = {
        val ast =
          TestRalphc.createTestContract(Ast.TypeId("MyContract"))

        ast.sourceIndex = astSourceIndex

        val warning =
          Warning(
            message = "Wassup!",
            sourceIndex = Some(SourceIndex(4, 5, Some(URI.create("file://warning.txt"))))
          )

        val code =
          CompiledCodeWrapper(TestRalphc.createCompiledContract(ast, warning))

        implicit val logger: ClientLogger =
          null // nothing should get logged

        val actual =
          RalphCompilerAccess.extractWarnings(code)

        actual should contain only warning
      }

      "ast SourceIndex is also defined" in {
        doTest(astSourceIndex = Some(SourceIndex(1, 2, Some(URI.create("file://blah.txt")))))
      }

      "ast SourceIndex is partially defined" in {
        doTest(astSourceIndex = Some(SourceIndex(1, 2, None)))
      }

      "ast SourceIndex is empty" in {
        doTest(astSourceIndex = None)
      }
    }
  }

}
