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

package org.alephium.ralph.lsp.access.compiler.ast.node

object Node {

  /** Create a [[Node]] with no children */
  @inline def apply[A](data: A): Node[A] =
    new Node(data, List.empty)

  /** Create a [[Node]] with children */
  @inline def apply[A](
      data: A,
      children: Seq[Node[A]]): Node[A] = {
    val parent = new Node(data, children)
    // set the parent for each child node to allow traversing up.
    children foreach (_._parent = Some(parent))
    parent
  }

}

/**
 * A [[Node]] represents a single position within a tree.
 *
 * Each node allows tree traversal in both forward and backward directions
 * using the functions like [[walkDown]], [[walkParents]] and others.
 *
 * @param data     The data stored in this node.
 * @param children This node's child nodes.
 * @tparam A Data type.
 */
case class Node[A] private (
    data: A,
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

  /**
   * Find the last node for which this predicate is true.
   *
   * This function is useful for find the closest node that contains a source-index.
   */
  def findLast(f: A => Boolean): Option[Node[A]] =
    self
      .walkDown
      .foldLeft(Option.empty[Node[A]]) {
        case (closest, nextNode) =>
          if (f(nextNode.data))
            Some(nextNode)
          else
            closest
      }

}
