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
        children
          .iterator
          .flatMap {
            child =>
              Iterator(child) ++
                child.walkDown
          }

      override def hasNext: Boolean =
        iter.hasNext

      override def next(): Node[A] =
        iter.next()
    }

  /** Walk parent nodes from the current node */
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
