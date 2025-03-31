// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref.multi

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

class GoToReferencesMultiSpec extends AnyWordSpec with Matchers with ScalaFutures {

  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val fileAccess: FileAccess   = FileAccess.disk
  implicit val logger: ClientLogger     = TestClientLogger
  implicit val ec: ExecutionContext     = ExecutionContext.Implicits.global

  "search local to the workspace" when {
    "the workspace is duplicate" in {
      goToRefMulti()(
        workspaces =
          /**
           * Workspace 1: Has the same code as Workspace 2, but it's not the one being searched.
           */
          ArraySeq(
            """
              |Abstract Contract Parent() { }
              |""".stripMargin,
            """
              |Contract Child() extends Parent() {
              |  fn function() -> () {}
              |}
              |""".stripMargin
          ),

        /**
         * Workspace 2: THe workspace being search.
         */
        ArraySeq(
          """
            |Abstract Contract Pare@@nt() { }
            |""".stripMargin,
          """
            |Contract Child() extends >>Parent<<() {
            |  fn function() -> () {}
            |}
            |""".stripMargin
        )
      )
    }
  }

  "search dependency" when {
    "workspaces are empty" in {
      goToRefMultiWithDependency()(
        dependencyID = DependencyID.Std,
        dependency = ArraySeq(
          // Dependency file 1
          """
            |Abstract Contract Pare@@nt() {}
            |
            |Abstract Contract Dependency1() extends >>Parent<<() {}
            |""".stripMargin,
          // Dependency file 2
          """
            |Abstract Contract Dependency2() extends >>Parent<<() {}
            |""".stripMargin
        ),
        workspaces = ArraySeq.empty
      )
    }

    "references exist only within the dependency" in {
      goToRefMultiWithDependency()(
        dependencyID = DependencyID.Std,
        dependency = ArraySeq(
          // Dependency file 1
          """
            |Abstract Contract Pare@@nt() {}
            |
            |Abstract Contract Dependency1() extends >>Parent<<() {}
            |""".stripMargin,
          // Dependency file 2
          """
            |Abstract Contract Dependency2() extends >>Parent<<() {}
            |""".stripMargin
        ),
        workspaces =
          /**
           * Workspace 1
           */
          ArraySeq(
            """
              |Contract One() extends ParentLocal() {
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
          """
            |Contract Two() extends ParentLocal() {
            |  fn function() -> () {
            |    let call = function()
            |  }
            |}
            |""".stripMargin
        )
      )
    }

    "references exist within and outside the dependency" when {
      "declaration are included" in {
        goToRefMultiWithDependency(settings = GoToRefMultiSetting(isIncludeDeclaration = true))(
          dependencyID = DependencyID.Std,

          /**
           * Dependencies also contain references to `Parent` which must be included in the search result.
           */
          dependency = ArraySeq(
            // Dependency file 1
            """
              |Abstract Contract >>Pare@@nt<<() {}
              |
              |Abstract Contract Dependency1() extends >>Parent<<() {}
              |""".stripMargin,
            // Dependency file 2
            """
              |Abstract Contract Dependency2() extends >>Parent<<() {}
              |""".stripMargin
          ),
          workspaces =
            /**
             * Workspace 1
             */
            ArraySeq(
              """
                |Contract One() extends >>Parent<<() {
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
            """
              |Contract Two() extends >>Parent<<() {
              |  fn function() -> () {
              |    let call = function()
              |  }
              |}
              |""".stripMargin
          )
        )
      }

      "declaration are excluded" in {
        goToRefMultiWithDependency()(
          dependencyID = DependencyID.Std,

          /**
           * Dependencies also contain references to `Parent` which must be included in the search result.
           */
          dependency = ArraySeq(
            // Dependency file 1
            """
              |Abstract Contract Pare@@nt() {}
              |
              |Abstract Contract Dependency1() extends >>Parent<<() {}
              |""".stripMargin,
            // Dependency file 2
            """
              |Abstract Contract Dependency2() extends >>Parent<<() {}
              |""".stripMargin
          ),
          workspaces =
            /**
             * Workspace 1
             */
            ArraySeq(
              """
                |Contract One() extends >>Parent<<() {
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
            """
              |Contract Two() extends >>Parent<<() {
              |  fn function() -> () {
              |    let call = function()
              |  }
              |}
              |""".stripMargin
          )
        )
      }
    }
  }

}
