package org.alephium.ralph.lsp.access.compiler.ast

object Node {
  @inline def apply[A](data: A): Node[A] =
    new Node(data, List.empty)

  @inline def apply[A](data: A, children: Seq[Node[A]]): Node[A] = {
    val parent = new Node(data, children)
    // set the parent for each child node to allow traversing up.
    children foreach (_._parent = Some(parent))
    parent
  }
}

case class Node[A] private(data: A,
                           children: Seq[Node[A]]) { self =>
  private var _parent: Option[Node[A]] =
    None

  def parent: Option[Node[A]] =
    _parent

  def headOption: Option[Node[A]] =
    parent
      .iterator
      .flatMap(_.walkParents)
      .foldLeft(Option.empty[Node[A]]) {
        case (_, next) =>
          Some(next)
      }

  /** Walk down from current node reaching all it's children and grand-children */
  def walkDown: Iterator[Node[A]] =
    new Iterator[Node[A]] {
      private val iter =
        Iterator(self) ++
          children
            .iterator
            .flatMap(_.walkDown)

      override def hasNext: Boolean =
        iter.hasNext

      override def next(): Node[A] =
        iter.next()
    }

  /** Reaches ALL nodes walking up from a node and then down, excluding this/self node and it's children */
  def walkUpDown: Iterator[A] =
    new Iterator[A] {
      private var lastParent = self

      private val iter: Iterator[A] =
        self
          .walkParents
          .map {
            parentNode =>
              // For the next parent, filter children that are not already processed in previous iteration (walking up)
              val newChildren =
                parentNode
                  .children
                  .filter(_ ne lastParent)

              lastParent = parentNode
              // Create a node copy without the previously processed parent node
              // Note: Due to this transformation, this tree is not longer the same as original tree.
              parentNode.copy(children = newChildren)
          }
          .flatMap(_.walkDown) // while walking up also traverse down each node.
          .map(_.data) // return data because the node's children are transformed above.

      override def hasNext: Boolean =
        iter.hasNext

      override def next(): A =
        iter.next()
    }

  def walkParents: Iterator[Node[A]] =
    new Iterator[Node[A]] {
      private val iter =
        self
          .parent
          .iterator
          .flatMap {
            parent =>
              Iterator(parent) ++
                parent.walkParents
          }

      override def hasNext: Boolean =
        iter.hasNext

      override def next(): Node[A] =
        iter.next()
    }
}
