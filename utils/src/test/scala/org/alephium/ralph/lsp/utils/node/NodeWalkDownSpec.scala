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
