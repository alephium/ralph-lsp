package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeWalkDownSpec extends AnyWordSpec with Matchers {

  "return non-empty" when {
    "there is only a root node" in {
      val root = Node("root")
      root.walkDown.map(_.data).toList should contain only "root"
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

      lastNode.walkDown.map(_.data).toList should contain only "Child-2"
    }
  }

  "include self" in {
    val allNodes =
      TestNode
        .root
        .walkDown
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
