// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef.multi

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.search.TestMultiCodeProvider._
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext

class GoToDefinitionMultiSpec extends AnyWordSpec with Matchers with ScalaFutures {

  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val fileAccess: FileAccess   = FileAccess.disk
  implicit val logger: ClientLogger     = TestClientLogger
  implicit val ec: ExecutionContext     = ExecutionContext.Implicits.global

  "search only the strict-ast" when {
    "soft-parser is disabled" when {
      "one workspace exists" in {
        goToDefMulti()(
          ArraySeq(
            // `Child` has valid syntax and is strict and soft parseable.
            """
              |Contract Child() extends Parent() {
              |  fn >>function<<() -> () {
              |    let call = func@@tion()
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
        )
      }

      "two workspaces exists" in {
        goToDefMulti()(
          /**
           * Workspace 1
           */
          ArraySeq(
            """
              |// This is a copy of workspace2's first source-file.
              |// This workspace should not be searched since the search is executed in Workspace2
              |Contract Child() extends Parent() {
              |  fn function() -> () {
              |    let call = function()
              |  }
              |}
              |""".stripMargin
          ),

          /**
           * Workspace 2
           */
          ArraySeq(
            // `Child` has valid syntax and is strict and soft parseable.
            """
              |Contract Child() extends Parent() {
              |  fn >>function<<() -> () {
              |    let call = func@@tion()
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
        )
      }

      "definition exists in dependency" when {
        "strict-parser is enabled" in {
          goToDefMultiWithDependency()(
            dependencyID = DependencyID.Std,
            dependency = ArraySeq(
              """
                |Contract Parent() {
                |  fn >>parent<<() -> () { }
                |}
                |""".stripMargin
            ),

            /**
             * Workspace 1
             */
            workspaces = ArraySeq(
              """
                |// This is a copy of workspace2's first source-file.
                |// This workspace should not be searched since the search is executed in Workspace2
                |Contract Child() extends Parent() {
                |  fn function() -> () {
                |    let call = function()
                |  }
                |}
                |""".stripMargin
            ),

            /**
             * Workspace 2
             */
            ArraySeq(
              // `Child` has valid syntax and is strict and soft parseable.
              """
                |import "std/file0"
                |
                |Contract Child() extends Parent() {
                |  fn function() -> () {
                |    let call = paren@@t()
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
          )
        }

        "soft-parser is enabled" in {
          goToDefMultiWithDependency(enableSoftParser = true)(
            dependencyID = DependencyID.Std,
            dependency = ArraySeq(
              """
                |Contract Parent() {
                |  fn >>parent<<() -> () { }
                |}
                |""".stripMargin
            ),

            /**
             * Workspace 1
             */
            workspaces = ArraySeq(
              """
                |// This is a copy of workspace2's first source-file.
                |// This workspace should not be searched since the search is executed in Workspace2
                |Contract Child() extends Parent() {
                |  fn function() -> () {
                |    let call = function()
                |  }
                |}
                |""".stripMargin
            ),

            /**
             * Workspace 2
             */
            ArraySeq(
              // `Child` has valid syntax and is strict and soft parseable.
              """
                |import "std/file0"
                |
                |Contract Child extends Parent {
                |  paren@@t
                |}
                |""".stripMargin,
              // `Parent` is only soft parseable.
              """
                |Contract Parent {
                |  fn function()
                |}
                |""".stripMargin
            )
          )
        }
      }
    }
  }

  "search both strict & Soft ASTs" when {
    "soft-parser is enabled" in {
      // When soft parsing is enabled, search results are returned for both `Parent` and `Child` contracts.
      // Even though `Parent` has syntax errors, it is soft-parsable.
      goToDefMulti(enableSoftParser = true)(
        /**
         * Workspace 1
         */
        // `Child` has valid syntax and is strict and soft parseable.
        ArraySeq(
          """
            |Contract Child() extends Parent() {
            |  fn >>function<<() -> () {
            |    let call = func@@tion()
            |  }
            |}
            |""".stripMargin,
          // `Parent` is only soft parseable.
          """
            |Contract Parent {
            |  fn >>function<<()
            |}
            |""".stripMargin
        ),

        /**
         * Workspace 2
         */
        ArraySeq(
          """
            |// Not accessed because it is in a different workspace
            |Contract Parent {
            |  fn function()
            |}
            |""".stripMargin
        )
      )
    }
  }

}
