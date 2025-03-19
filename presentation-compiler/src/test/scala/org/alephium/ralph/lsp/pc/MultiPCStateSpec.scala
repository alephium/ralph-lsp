// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.util.Random

class MultiPCStateSpec extends AnyWordSpec with Matchers {

  "findContains" should {
    def testFind(
        fileURI: String,
        foldURI: String): Option[PCState] =
      TestMultiPCState
        .genMultiPCState(Iterable(Paths.get(foldURI).toUri))
        .findContains(Paths.get(fileURI).toUri)

    "return None" when {
      "file does not belong a folder" in {
        testFind(
          foldURI = "/parent/child",
          fileURI = "/blah" // `blah` has no workspace
        ) shouldBe empty
      }

      "file is the parent directory (not contained within the folder)" in {
        testFind(
          foldURI = "/parent/child",
          fileURI = "/parent" // `parent` is not contained within `/parent/child`
        ) shouldBe empty
      }
    }

    "return Some" when {
      "fileURI matches exactly to the parentURI" in {
        val pcState =
          testFind(
            foldURI = "/parent/child",
            fileURI = "/parent/child"
          ).value

        pcState.workspace.workspaceURI shouldBe Paths.get("/parent/child").toUri
      }

      "fileURI is contained within the parentURI" in {
        val pcState =
          testFind(
            foldURI = "/parent/child",
            fileURI = "/parent/child/code.ral"
          ).value

        pcState.workspace.workspaceURI shouldBe Paths.get("/parent/child").toUri
      }
    }
  }

  "removeContains" should {
    def testRemove(
        foldURI: Iterable[String],
        fileURI: String): Option[(MultiPCState, ArraySeq[PCState])] = {
      val parent =
        foldURI.map(Paths.get(_).toUri)

      TestMultiPCState
        .genMultiPCState(parent)
        .remove(Paths.get(fileURI).toUri)
    }

    "return None" when {
      "file does not belong a folder" in {
        testRemove(
          foldURI = Array("/parent/child"),
          fileURI = "/blah" // `blah` has no workspace
        ) shouldBe empty
      }

      "file is the parent directory (not contained within the folder)" in {
        testRemove(
          foldURI = Array("/parent/child"),
          fileURI = "/parent" // `parent` is not contained within `/parent/child`
        ) shouldBe empty
      }

      "fileURI is contained within the parentURI but is not an exact match" in {
        testRemove(
          foldURI = Array("/parent/child"),
          fileURI = "/parent/child/code.ral"
        ) shouldBe empty
      }
    }

    "return Some" when {
      "fileURI matches exactly to the parentURI" when {
        "only on workspace exists" in {
          val (multiPCState, removedStated) =
            testRemove(
              foldURI = Array("/parent/child"),
              fileURI = "/parent/child"
            ).value

          // no PCStates left
          multiPCState.states shouldBe empty

          // assert removes
          removedStated should have size 1
          removedStated.head.workspace.workspaceURI shouldBe Paths.get("/parent/child").toUri
        }

        "multiple workspaces exist" in {
          val (multiPCState, removedStated) =
            testRemove(
              foldURI = Random.shuffle(
                List(
                  "/parent/child",
                  "/parent",
                  "/up/down",
                  "/this/that"
                )
              ),
              fileURI = "/parent/child"
            ).value

          val expected =
            Seq("/parent", "/up/down", "/this/that") map {
              path =>
                Paths.get(path).toUri
            }

          // other PCStates left
          val actual = multiPCState.states.map(_.workspace.workspaceURI)
          actual should contain theSameElementsAs expected

          // assert removes
          removedStated should have size 1
          removedStated.head.workspace.workspaceURI shouldBe Paths.get("/parent/child").toUri

        }
      }
    }
  }

}
