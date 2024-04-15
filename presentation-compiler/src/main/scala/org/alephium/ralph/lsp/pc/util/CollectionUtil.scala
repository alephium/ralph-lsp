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

package org.alephium.ralph.lsp.pc.util

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
