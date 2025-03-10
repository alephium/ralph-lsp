// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils.node

import org.alephium.ralph.lsp.utils.Node

object TestNode {

  val root: Node[String, String] =
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
