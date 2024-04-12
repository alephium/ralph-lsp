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

object TestNode {

  val root: Node[String] =
    Node(
      "1",
      Seq(
        Node(
          "1-1",
          Seq(
            Node(
              "1-1-1",
              Seq(
                Node("1-1-1-1"),
                Node("1-1-1-2")
              )
            )
          )
        ),
        Node(
          "1-2",
          Seq(
            Node("1-2-1"),
            Node("1-2-2")
          )
        ),
        Node(
          "1-3",
          Seq(
            Node(
              "1-3-1"
            ),
            Node(
              "1-3-2",
              Seq(
                Node(
                  "1-3-2-1"
                ),
                Node(
                  "1-3-2-2",
                  Seq(
                    Node("1-3-2-2-1"),
                    Node("1-3-2-2-2")
                  )
                )
              )
            )
          )
        ),
        Node(
          "1-4",
          Seq(
            Node("1-4-1"),
            Node("1-4-2")
          )
        )
      )
    )

}
