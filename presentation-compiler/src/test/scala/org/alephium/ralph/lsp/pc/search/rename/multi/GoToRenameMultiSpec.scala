// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename.multi

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.access.util.TestCodeUtil
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.search.TestMultiCodeProvider._
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext

class GoToRenameMultiSpec extends AnyWordSpec with Matchers {

  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val fileAccess: FileAccess   = FileAccess.disk
  implicit val logger: ClientLogger     = TestClientLogger
  implicit val ec: ExecutionContext     = ExecutionContext.Implicits.global

  "always rename local to the workspace" in {
    goToRenameMulti(
      /**
       * Workspace1
       */
      ArraySeq(
        """
          |Abstract Contract >>Pare@@nt<<() { }
          |""".stripMargin,
        """
          |Abstract Contract Child() extends >>Parent<<() { }
          |""".stripMargin
      ),

      /**
       * Workspace2: `Parent` is defined in Workspace1. Workspace2 should not be renamed.
       */
      ArraySeq(
        """
          |Abstract Contract Child() extends Parent() { }
          |""".stripMargin
      )
    )
  }

  "renaming within dependency code" should {
    "not be allowed" when {
      "dependency code is being renamed directly" in {
        goToRenameMultiWithDependency(
          dependencyID = DependencyID.Std,
          dependency = ArraySeq(
            """
              |Abstract Contract Pare@@nt() { }
              |""".stripMargin,
            """
              |Abstract Contract Child() extends Parent() { }
              |""".stripMargin
          ),

          /**
           * `Parent` is defined as a dependency, it should not be renamed.
           */
          workspaces = ArraySeq(
            """
              |import "std/file0"
              |
              |Abstract Contract Child() extends Parent() { }
              |""".stripMargin
          )
        )
      }

      "dependency code is being renamed indirectly via workspace code" in {
        val dependencyID =
          DependencyID.Std

        val dependency =
          ArraySeq(
            """
              |Abstract Contract >>Parent<<() { }
              |""".stripMargin,
            """
              |Abstract Contract Child() extends Parent() { }
              |""".stripMargin
          )

        val workspaces =
          ArraySeq(
            """
              |import "std/file0"
              |
              |Abstract Contract Child() extends Pare@@nt() { }
              |""".stripMargin
          )

        /**
         * First, assert that go-to definition works as expected.
         */
        goToDefMultiWithDependency()(
          dependencyID = dependencyID,
          dependency = dependency,
          workspaces = workspaces
        )

        /**
         * Second, remove the range marker `>><<` and assert renaming is not applied.
         */
        val dependencyWithoutRange =
          dependency map TestCodeUtil.clearTestMarkers

        dependencyWithoutRange.foreach(_.contains(">>") shouldBe false)

        goToRenameMultiWithDependency(
          dependencyID = dependencyID,
          dependency = dependencyWithoutRange,
          workspaces = workspaces
        )
      }
    }
  }

}
