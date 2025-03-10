// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils

import java.util
import scala.collection.immutable.ArraySeq

object CollectionUtil {

  /**
   * Converts an iterator to a Java ArrayList.
   *
   * @param iterator The iterator to convert.
   * @tparam A The type of elements.
   * @return A Java ArrayList containing the elements from the iterator.
   */
  def toJavaList[A](iterator: Iterator[A]): util.ArrayList[A] = {
    val javaList = new util.ArrayList[A]()
    iterator.foreach(javaList.add)
    javaList
  }

  implicit class CollectionUtilImplicits[+A](val collection: ArraySeq[A]) extends AnyVal {

    /** Update the current element or insert if new element */
    def put[B >: A](update: B)(implicit ordering: Ordering[B]): ArraySeq[B] = {
      val index = collection.indexWhere(ordering.equiv(_, update))
      if (index >= 0)
        collection.updated(index, update)
      else
        collection appended update
    }

    def putIfEmpty[B >: A](update: B)(implicit ordering: Ordering[B]): ArraySeq[B] = {
      val index = collection.indexWhere(ordering.equiv(_, update))
      if (index == -1)
        collection appended update
      else
        collection
    }

    /**
     * Merge elements from new collection into existing collection favouring
     * duplicates from new collection.
     */
    def merge[B >: A](newCollection: ArraySeq[B])(implicit ordering: Ordering[B]): ArraySeq[B] = {
      val oldCollection = // old collection with new-items removed.
        collection filterNot { // filter items that do not exist in the new collection
          existingItem =>
            newCollection exists {
              newItem =>
                ordering.equiv(existingItem, newItem)
            }
        }

      oldCollection ++ newCollection
    }

  }

}
