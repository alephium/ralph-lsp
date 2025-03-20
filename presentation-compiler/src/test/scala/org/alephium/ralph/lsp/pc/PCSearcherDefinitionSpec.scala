// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation.GoToDef
import org.alephium.ralph.lsp.pc.workspace.{TestWorkspace, WorkspaceState}
import org.alephium.ralph.lsp.utils.IsCancelled
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.util.TestCodeUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.EitherValues._
import org.scalatest.concurrent.PatienceConfiguration.Timeout

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class PCSearcherDefinitionSpec extends AnyWordSpec with Matchers with ScalaFutures {

  implicit val compiler: CompilerAccess      = CompilerAccess.ralphc
  implicit val fileAccess: FileAccess        = FileAccess.disk
  implicit val logger: TestClientLogger.type = TestClientLogger
  implicit val ec: ExecutionContext          = ExecutionContext.Implicits.global

  "search only the strict-ast" when {
    def runTest(enableSoftParser: Boolean): (Either[CompilerMessage.Error, ArraySeq[GoToDef]], WorkspaceState.IsParsed) = {
      // Create a workspace with:
      // - one source file `Child` that has valid syntax, i.e. it's both soft and strict parseable
      // - one source file `Parent` that has invalid syntax, i.e. it's only soft parseable
      val workspace =
        TestWorkspace
          .genParsed(
            // `Child` has valid syntax and is strict and soft parseable.
            """
              |Contract Child() extends Parent() {
              |  fn function() -> () {
              |    let call = function()
              |  }
              |}
              |""".stripMargin,
            // `Parent` is only soft parseable.
            """
              |Contract Parent {
              |  fn function()
              |}
              |""".stripMargin
          )
          .sample
          .value

      // the workspace contains two source-files
      workspace.sourceCode should have size 2

      // both source files are code-aware
      val sourceFiles =
        workspace.sourceCode.map(_.asInstanceOf[SourceCodeState.IsCodeAware])

      // location to click
      val click =
        TestCodeUtil.lineRange(
          """
            |Contract Child() extends Parent() {
            |  fn function() -> () {
            |    let call = functi>>o<<n()
            |  }
            |}
            |""".stripMargin
        )

      val pcStates =
        TestPCStates.genPCStates(workspace)

      val result =
        PCSearcher
          .definition(
            fileURI = sourceFiles.head.fileURI,
            line = click.from.line,
            character = click.from.character,
            enableSoftParser = enableSoftParser,
            // format: off
            isCancelled = IsCancelled(() => false), // format: on
            pcStates = pcStates
          )
          .futureValue(Timeout(5.seconds))

      TestWorkspace delete workspace

      (result, workspace)
    }

    "soft-parser is disabled" in {
      // When soft-parse is disabled, search results are only returned from the `Child` contract
      // which has 100% valid syntax.
      val (result, workspace) =
        runTest(enableSoftParser = false)

      val locations =
        result
          .value
          .map(_.asInstanceOf[SourceLocation.NodeStrict[Ast.Positioned]]) // only strict-ast results are returned

      val sourceIndexes = locations.map(_.ast.sourceIndex.value)

      val expectedIndex =
        indexOf {
          """
            |Contract Child() extends Parent() {
            |  fn >>function<<() -> () {
            |    let call = function()
            |  }
            |}
            |""".stripMargin
        }.copy(fileURI = Some(workspace.sourceCode.head.fileURI))

      sourceIndexes should contain only expectedIndex
    }

    "soft-parser is enabled" in {
      // When soft parsing is enabled, search results are returned for both `Parent` and `Child` contracts.
      // Even though `Parent` has syntax errors, it is soft-parsable.
      val (result, _) =
        runTest(enableSoftParser = true)

      val actualIndexes =
        result
          .value
          .map(_.index.value)

      val expectedIndex1 =
        indexOf {
          """
            |Contract Child() extends Parent() {
            |  fn >>function<<() -> () {
            |    let call = function()
            |  }
            |}
            |""".stripMargin
        }

      val expectedIndex2 =
        indexOf {
          """
            |Contract Parent {
            |  fn >>function<<()
            |}
            |""".stripMargin
        }

      actualIndexes should contain only (expectedIndex1, expectedIndex2)
    }
  }

}
