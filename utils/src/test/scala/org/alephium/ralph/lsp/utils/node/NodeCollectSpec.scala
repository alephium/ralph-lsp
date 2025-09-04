// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils.node

import org.alephium.ralph.lsp.utils.Node
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeCollectSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "the only node (root) is filtered out" when {
      "root node has no children" in {
        val root = Node("root")

        root
          .collect {
            // filter out the root node
            case node @ Node(string, _) if string != root.data =>
              node
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
          .collect {
            case node @ Node("blah", _) =>
              node
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
        .collect {
          node =>
            node
        }
        .map(_.data)
        .toList should contain only "root"
    }

    "there are child nodes" in {
      val root =
        Node(
          "Parent",
          Seq(
            Node(
              "Child-1",
              Seq(
                Node("Child-1-1")
              )
            ),
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

      val result =
        root
          .collect {
            case child @ Node("Child-1", _) =>
              child

            case child @ Node("Child-2-1", _) =>
              child

            case child @ Node("Child-3", _) =>
              child
          }
          .map(_.data)
          .toList

      result should contain inOrderElementsOf List("Child-1", "Child-2-1", "Child-3")
    }

    "collection is unordered, the output should still be in order of the node's position" in {
      val result =
        TestNode
          .root
          .collect {
            case node @ Node("1-4", _) =>
              node

            case node @ Node("1-1-1", _) =>
              node

            case node @ Node("1-3-1", _) =>
              node
          }
          .map(_.data)
          .toList

      result should contain inOrderElementsOf List("1-1-1", "1-3-1", "1-4")
    }

    "only the last node is collected" in {
      val result =
        TestNode
          .root
          .collect {
            case node @ Node("1-4-2", _) =>
              node
          }
          .map(_.data)
          .toList

      result should contain only "1-4-2"
    }
  }

}
