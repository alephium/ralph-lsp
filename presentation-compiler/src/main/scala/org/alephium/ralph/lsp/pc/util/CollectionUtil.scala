package org.alephium.ralph.lsp.pc.util

import scala.collection.immutable.ArraySeq

object CollectionUtil {

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
