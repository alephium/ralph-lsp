// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils.node

import org.alephium.ralph.lsp.utils.Node
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeFilterSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "the only node (root) is filtered out" when {
      "root node has no children" in {
        val root = Node("root")

        root
          .filter {
            node =>
              node.data shouldBe "root"
              false
          }
          .map(_.data)
          .toList shouldBe empty
      }

      "root node has children" in {
        val root =
          Node(
            "Parent",
            Seq(
              Node("Child-1"),
              Node(
                "Child-2",
                Seq(
                  Node("Child-2-1"),
                  Node("Child-2-2")
                )
              )
            )
          )

        root
          .filter {
            case Node("Parent", _) =>
              false // Drop the parent

            case other =>
              // Because the parent is filtered out, its children should not have been processed.
              fail(s"${other.data} should not have been processed")
          }
          .map(_.data)
          .toList shouldBe empty
      }
    }

  }

  "return non-empty" when {
    "there is only a root node" in {
      val root = Node("root")

      root
        .filter {
          _ =>
            true
        }
        .map(_.data)
        .toList should contain only "root"
    }

    "there are child nodes" in {
      val root =
        Node(
          "Parent",
          Seq(
            Node("Child-1"),
            Node(
              "Child-2",
              Seq(
                Node("Child-2-1"),
                Node("Child-2-2")
              )
            ),
            Node("Child-3")
          )
        )

      root
        .filter {
          case Node("Child-2", _) =>
            // Drop Child-2 and expect all of its children must also be dropped
            false

          case _ =>
            true // Keep everything else
        }
        .map(_.data)
        .toList should contain only ("Parent", "Child-1", "Child-3")

    }

    "only the root and the last parent node are kept" in {
      TestNode
        .root
        .filter {
          case Node("1", _) =>
            true

          case Node("1-4", _) =>
            true

          case Node("1-4-2", _) =>
            true

          case _ =>
            false // Drop everything else
        }
        .map(_.data)
        .toList should contain only ("1", "1-4", "1-4-2")

    }
  }

}
