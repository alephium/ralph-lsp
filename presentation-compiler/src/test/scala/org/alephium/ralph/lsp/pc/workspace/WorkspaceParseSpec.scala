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

import org.alephium.ralph.lsp.TestCode
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.parse]] function.
 */
class WorkspaceParseSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "fail" when {
    "some or all source-code fails" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      // generate a build file with at least one bad source-code
      forAll(TestBuild.genCompiledWithSourceCode(code = TestCode.genBadCode(), minSourceCount = 1)) {
        case (build, _sourceCode) =>
          val sourceCode =
            _sourceCode.to(ArraySeq)

          // create a workspace for the build file
          val workspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = sourceCode
            )

          // parse the un-compiled workspace
          val actualWorkspace =
            Workspace.parse(workspace)

          // parse all source-code
          val expectedSourceCodeStates =
            sourceCode
              .map(SourceCode.parse)
              .map(_.asInstanceOf[SourceCodeState.ErrorParser])

          val expectedWorkspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = expectedSourceCodeStates
            )

          actualWorkspace shouldBe expectedWorkspace

          TestWorkspace delete actualWorkspace
      }
    }
  }

  "pass" when {
    "all source-code is OK" in {
      implicit val file: FileAccess =
        FileAccess.disk

      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      // generate a build file with source-code
      forAll(TestBuild.genCompiledWithSourceCode()) {
        case (build, _sourceCode) =>
          val sourceCode =
            _sourceCode.to(ArraySeq)

          // create a workspace for the build file
          val workspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = sourceCode
            )

          // parse the un-compiled workspace
          val actualWorkspace =
            Workspace.parse(workspace)

          // parse all source-code
          val expectedSourceCodeStates =
            sourceCode
              .map(SourceCode.parse)
              .map(_.asInstanceOf[SourceCodeState.Parsed])

          val expectedWorkspace =
            WorkspaceState.Parsed(
              build = build,
              sourceCode = expectedSourceCodeStates
            )

          actualWorkspace shouldBe expectedWorkspace

          TestWorkspace delete actualWorkspace
      }
    }
  }

}
