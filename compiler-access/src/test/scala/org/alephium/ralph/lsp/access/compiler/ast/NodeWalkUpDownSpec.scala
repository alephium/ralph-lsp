package org.alephium.ralph.lsp.access.compiler.ast

import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeWalkUpDownSpec extends AnyWordSpec with Matchers {

  "return empty walking up from root node" when {
    "there are no child nodes" in {
      val root = Node("root")
      root.walkUpDown shouldBe empty
    }

    "there are child nodes" in {
      val root =
        Node(
          "Parent-1",
          Seq(
            Node("Child-1"),
            Node("Child-2")
          )
        )

      root.data shouldBe "Parent-1"

      root.walkUpDown shouldBe empty
    }
  }

  "return non-empty" when {
    "from 1-3-2-2-1" in {
      val fromNode =
        TestNode
          .root
          .walkDown
          .find(_.data == "1-3-2-2-1")
          .value

      val expectedNodes =
        List(
          "1-3-2-2",
          "1-3-2-2-2",
          "1-3-2",
          "1-3-2-1",
          "1-3",
          "1-3-1",
          "1",
          "1-1",
          "1-1-1",
          "1-1-1-1",
          "1-1-1-2",
          "1-2",
          "1-2-1",
          "1-2-2",
          "1-4",
          "1-4-1",
          "1-4-2"
        )

      fromNode.walkUpDown.toList shouldBe expectedNodes
    }

    "from 1-3-2-2-2" in {
      val fromNode =
        TestNode
          .root
          .walkDown
          .find(_.data == "1-3-2-2-2")
          .value

      val expectedNodes =
        List(
          "1-3-2-2",
          "1-3-2-2-1",
          "1-3-2",
          "1-3-2-1",
          "1-3",
          "1-3-1",
          "1",
          "1-1",
          "1-1-1",
          "1-1-1-1",
          "1-1-1-2",
          "1-2",
          "1-2-1",
          "1-2-2",
          "1-4",
          "1-4-1",
          "1-4-2"
        )

      fromNode.walkUpDown.toList shouldBe expectedNodes
    }

    "1-4-2 (bottom last node)" in {
      val fromNode =
        TestNode
          .root
          .walkDown
          .find(_.data == "1-4-2")
          .value

      val expectedNodes =
        List(
          "1-4",
          "1-4-1",
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
          "1-3-2-2-2"
        )

      fromNode.walkUpDown.toList shouldBe expectedNodes
    }
  }
}
