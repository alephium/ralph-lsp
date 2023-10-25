package org.alephium.ralph.lsp.pc.util

import scala.collection.immutable.ArraySeq

object CollectionUtil {

  implicit class CollectionUtilImplicits[T](val collection: ArraySeq[T]) extends AnyVal {

    /** Update the current element or insert if new element */
    def put(update: T)(implicit ordering: Ordering[T]): ArraySeq[T] = {
      val index = collection.indexWhere(ordering.equiv(_, update))
      if (index >= 0)
        collection.updated(index, update)
      else
        collection appended update
    }

    def putIfEmpty(update: T)(implicit ordering: Ordering[T]): ArraySeq[T] = {
      val index = collection.indexWhere(ordering.equiv(_, update))
      if (index == -1)
        collection appended update
      else
        collection
    }
  }
}
