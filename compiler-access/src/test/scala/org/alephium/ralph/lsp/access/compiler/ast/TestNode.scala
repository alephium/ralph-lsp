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
                Node("1-1-1-2"),
              )
            )
          )
        ),
        Node(
          "1-2",
          Seq(
            Node("1-2-1"),
            Node("1-2-2"),
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
                    Node("1-3-2-2-2"),
                  )
                ),
              )
            )
          )
        ),
        Node(
          "1-4",
          Seq(
            Node("1-4-1"),
            Node("1-4-2"),
          )
        )
      )
    )
}
