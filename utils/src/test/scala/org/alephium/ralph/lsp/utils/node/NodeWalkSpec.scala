// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils.node

import org.alephium.ralph.lsp.utils.Node
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeWalkSpec extends AnyWordSpec with Matchers {

  "return non-empty" when {
    "there is only a root node" in {
      val root = Node("root")
      root.walk.map(_.data).toList should contain only "root"
    }

    "there are child nodes but walking from last node" in {
      val root =
        Node(
          "Parent-1",
          Seq(
            Node("Child-1"),
            Node("Child-2")
          )
        )

      val lastNode =
        root.children.last

      lastNode.data shouldBe "Child-2"

      lastNode.walk.map(_.data).toList should contain only "Child-2"
    }
  }

  "include self" in {
    val allNodes =
      TestNode
        .root
        .walk
        .map(_.data)
        .toList

    allNodes shouldBe
      List(
        "1",
        "1-1",
        "1-1-1",
        "1-1-1-1",
        "1-1-1-2",
        "1-2",
        "1-2-1",
        "1-2-2",
        "1-3",
        "1-3-1",
        "1-3-2",
        "1-3-2-1",
        "1-3-2-2",
        "1-3-2-2-1",
        "1-3-2-2-2",
        "1-4",
        "1-4-1",
        "1-4-2"
      )
  }

}
