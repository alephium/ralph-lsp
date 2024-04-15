// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.scalatest.OptionValues._
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

  "return non-empty" when {
    "1-3-2-2-1 and 1-3-2-2-2 (middle nodes)" in {
      val midNode1 =
        TestNode
          .root
          .walkDown
          .find(_.data == "1-3-2-2-1")
          .value

      val midNode2 =
        TestNode
          .root
          .walkDown
          .find(_.data == "1-3-2-2-2")
          .value

      // Note: midNode1 and midNode2 are siblings so their parents must be the same
      val expectedParents =
        List(
          "1-3-2-2",
          "1-3-2",
          "1-3",
          "1"
        )

      Array(midNode1, midNode2) foreach {
        node =>
          node.walkParents.map(_.data).toList shouldBe expectedParents
      }
    }

    "1-4-2 (bottom last node)" in {
      val lastNode =
        TestNode
          .root
          .walkDown
          .find(_.data == "1-4-2")
          .value

      lastNode.walkParents.map(_.data).toList shouldBe
        List(
          "1-4",
          "1"
        )
    }
  }

}
