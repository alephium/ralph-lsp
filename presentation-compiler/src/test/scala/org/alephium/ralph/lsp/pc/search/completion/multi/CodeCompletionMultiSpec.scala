// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion.multi

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.search.TestMultiCodeProvider._
import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext

class CodeCompletionMultiSpec extends AnyWordSpec with Matchers with ScalaFutures {

  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val fileAccess: FileAccess   = FileAccess.disk
  implicit val logger: ClientLogger     = TestClientLogger
  implicit val ec: ExecutionContext     = ExecutionContext.Implicits.global

  "suggest only within the searched workspace" when {
    "both workspaces contain the same type `Parent` with different function names" in {
      val result =
        suggestMulti(
          /**
           * Workspace1
           */
          ArraySeq(
            """
              |Contract Workspace1() {
              |  fn function(parent: Parent) -> () {
              |    let call = parent.func@@()
              |  }
              |}
              |""".stripMargin,
            """
              |Contract Parent() {
              |  fn function_one() -> () {} 
              |}
              |""".stripMargin
          ),

          /**
           * Workspace2 also contains `Parent`. But its function `function_two` should not be suggested in completion.
           */
          ArraySeq(
            """
              |Contract Parent() {
              |  fn function_two() -> () {} 
              |}
              |""".stripMargin
          )
        )

      val suggestions =
        result.collect {
          case function: Suggestion.FuncDef =>
            function.node.ast.name
        }

      // only the function name from Workspace1 is suggested.
      suggestions should contain only "function_one"
    }
  }

  "suggest only within the searched workspace and its dependency" when {
    "the dependency and the second workspace both contain the same type `Parent` with different function names" in {
      val result =
        suggestMultiWithDependency(
          dependencyID = DependencyID.Std,
          dependency = ArraySeq(
            """
              |Contract Parent() {
              |  fn function_one() -> () {} 
              |}
              |""".stripMargin
          ),
          workspaces =
            /**
             * Workspace1
             */
            ArraySeq(
              """
                |import "std/file0"
                |
                |Contract Workspace1() {
                |  fn function(parent: Parent) -> () {
                |    let call = parent.func@@()
                |  }
                |}
                |""".stripMargin
            ),

          /**
           * Workspace2 also contains `Parent`. But its function `function_two` should not be suggested in completion.
           */
          ArraySeq(
            """
              |Contract Parent() {
              |  fn function_two() -> () {}
              |}
              |""".stripMargin
          )
        )

      val suggestions =
        result.collect {
          case function: Suggestion.FuncDef =>
            function.node.ast.name
        }

      // only the functions from the dependency and Workspace1 are suggested.
      suggestions should contain only "function_one"
    }
  }

}
