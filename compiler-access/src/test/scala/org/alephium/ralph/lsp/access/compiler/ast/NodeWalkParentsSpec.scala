package org.alephium.ralph.lsp.access.compiler.ast

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeWalkParentsSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "there are no child nodes" in {
      val root = Node("root")
      root.walkParents shouldBe empty
    }

    "there are child nodes but walking from root node" in {
      val root =
        Node(
          "Parent-1",
          Seq(
            Node("Child-1"),
            Node("Child-2")
          )
        )

      // start from root would still result in empty
      root.walkParents shouldBe empty

      // But starting from Child-2, the Parent-1 is returned
      val lastChild = root.children.last
      lastChild.data shouldBe "Child-2"
      lastChild.walkParents.map(_.data).toList should contain only "Parent-1"
    }
  }
}
