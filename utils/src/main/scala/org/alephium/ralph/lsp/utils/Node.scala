// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils

import scala.annotation.unchecked.uncheckedVariance

object Node {

  /**
   * Constructs a new `Node` with the given data and no children.
   *
   * @param data The data to be stored in the new node.
   * @tparam A The data type of the new node.
   * @tparam B The data type of the child nodes, which must be a super type of `A`.
   * @return A new `Node` instance with the specified data and no children.
   */
  @inline def apply[A, B >: A](data: A): Node[A, B] =
    new Node(data, List.empty)

  /**
   * Constructs a new `Node` for the provided data and children.
   *
   * @param data     The data to be stored in the new node.
   * @param children The child nodes of the new node.
   * @tparam A The data type of the new node.
   * @tparam B The data type of the child nodes, which must be a super type of `A`.
   * @return A new `Node` instance with the specified data and children.
   */
  @inline def apply[A, B >: A](
      data: A,
      children: Seq[Node[B, B]]): Node[A, B] = {
    val parent =
      new Node[A, B](
        data = data,
        children = children
      )
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
 * @tparam A Data type of the current node.
 * @tparam B Data type of every other node, which is a super type of [[A]].
 */
case class Node[+A, B] private (
    data: A,
    children: Seq[Node[B, B]]) { self =>

  private var _parent: Option[Node[B, B]] =
    None

  def parent: Option[Node[B, B]] =
    _parent

  def headOption: Option[Node[B, B]] =
    parent
      .iterator
      .flatMap(_.walkParents)
      .foldLeft(Option.empty[Node[B, B]]) {
        case (_, next) =>
          Some(next)
      }

  /** Walk down from current node reaching all it's children and grand-children */
  def walkDown: Iterator[Node[B, B]] =
    new Iterator[Node[B, B]] {

      private val iter =
        Iterator.single(self.asInstanceOf[Node[B, B]]) ++
          children
            .iterator
            .flatMap(_.walkDown)

      override def hasNext: Boolean =
        iter.hasNext

      override def next(): Node[B, B] =
        iter.next()

    }

  /**
   * Walks down from the current node, applying the given predicate to each node (including self) and its children.
   *
   * When a parent node does not satisfy the predicate, it is filtered-out along with all of its children.
   */
  def filterDown(f: Node[B, B] => Boolean): Iterator[Node[B, B]] =
    new Iterator[Node[B, B]] {

      private val iter = {
        val selfCasted =
          self.asInstanceOf[Node[B, B]]

        if (f(selfCasted))
          Iterator.single(selfCasted) ++
            children
              .iterator
              .flatMap(_.filterDown(f))
        else
          Iterator.empty
      }

      override def hasNext: Boolean =
        iter.hasNext

      override def next(): Node[B, B] =
        iter.next()

    }

  def walkParents: Iterator[Node[B, B]] =
    new Iterator[Node[B, B]] {

      private val iter =
        self
          .parent
          .iterator
          .flatMap {
            parent =>
              Iterator.single(parent) ++
                parent.walkParents
          }

      override def hasNext: Boolean =
        iter.hasNext

      override def next(): Node[B, B] =
        iter.next()

    }

  /**
   * Find the last node for which this predicate is true.
   *
   * This function is useful for find the closest node that contains a source-index.
   *
   * Use [[findLastChild]] for depth-first search.
   */
  def findLast(f: B => Boolean): Option[Node[B, B]] =
    self
      .walkDown
      .foldLeft(Option.empty[Node[B, B]]) {
        case (closest, nextNode) =>
          if (f(nextNode.data))
            Some(nextNode)
          else
            closest
      }

  /**
   * Depth-first search.
   *
   * Finds the last node for which the predicate is true, skipping the node
   * and its children if the predicate fails for the parent node.
   */
  def findLastChild(f: Node[B, B] => Boolean): Option[Node[B, B]] = {
    val selfCasted =
      self.asInstanceOf[Node[B, B]]

    if (f(selfCasted))
      children
        .iterator
        .flatMap(_.findLastChild(f))
        .foldLeft(Some(selfCasted)) {
          case (_, next) =>
            Some(next)
        }
    else
      None
  }

  /**
   * Upcasts this node's data type [[A]] to a narrower type.
   *
   * @param data The data of the current node with a narrower type.
   * @tparam C The type to upcast to.
   * @return This node with the data type upcasted.
   */
  def upcast[C <: A @uncheckedVariance](data: C): Node[C, B] =
    // upcasting should not break tree hierarchy. The data should match the existing data in this node.
    if (self.data != data)
      // FIXME: Throwing an Exception here is not very nice, but currently necessary.
      //        Pattern matching on the Node type does not return narrower types, requiring this solution until a better
      //        solution using the type-system is implemented.
      throw new IllegalArgumentException(s"Invalid upcast. ${data.getClass.getSimpleName} cannot be casted to ${self.data.getClass.getSimpleName}.")
    else
      self.asInstanceOf[Node[C, B]]

  /**
   * Builds a string representation of the tree structure of the current [[Node]]
   * and its children, formatted according to the provided function.
   *
   * @param format Converts a [[Node]] of type `B` into a displayable string format.
   * @param tab    Number of spaces to use for indentation.
   * @return String representing the tree structure.
   */
  def toStringTree(
      format: B => String,
      tab: Int = 2): String = {
    def toStringTree(
        node: Node[B, B],
        index: Int): Seq[String] = {
      val children =
        node.children flatMap {
          child =>
            toStringTree(child, index + tab)
        }

      val spaces         = " " * index
      val thisNodeString = spaces + format(node.data)
      thisNodeString +: children
    }

    val strings =
      toStringTree(
        node = self.asInstanceOf[Node[B, B]],
        index = 0
      )

    strings.mkString("\n")
  }

}
