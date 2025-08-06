// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils

import java.lang.ref.WeakReference
import java.util

/**
 * The value are also stored as [[WeakReference]]s to handle cyclic cases
 * where the value stores a strong reference back to the key.
 */
abstract class WeakHashMapBase[K, V](cache: util.WeakHashMap[K, WeakReference[V]]) {

  protected def getOrPut(
      key: K,
      value: => V): V =
    cache.synchronized {
      val ref =
        cache.get(key)

      val existingValue =
        if (ref != null)
          ref.get()
        else
          null

      if (existingValue != null) {
        existingValue.asInstanceOf[V]
      } else {
        val newValue = value
        cache.put(key, new WeakReference(newValue))
        newValue
      }
    }

}
