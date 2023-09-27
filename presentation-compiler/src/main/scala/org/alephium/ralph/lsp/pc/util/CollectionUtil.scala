package org.alephium.ralph.lsp.pc.util

import scala.collection.immutable.ArraySeq

object CollectionUtil {

  def updateOrAdd[T](collection: ArraySeq[T], update: T)(implicit ordering: Ordering[T]): ArraySeq[T] = {
    val index = collection.indexWhere(ordering.equiv(_, update))
    if (index >= 0)
      collection.updated(index, update)
    else
      collection appended update
  }

}
