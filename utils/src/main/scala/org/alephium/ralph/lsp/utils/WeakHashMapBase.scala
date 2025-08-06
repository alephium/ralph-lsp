// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils

import org.alephium.ralph.lsp.utils.log.StrictImplicitLogging

import java.lang.ref.WeakReference
import java.util

abstract class WeakHashMapBase[K, V](cache: util.WeakHashMap[K, WeakReference[V]]) extends StrictImplicitLogging {

  def getOrPut(
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
        val newCache = value
        cache.put(key, new WeakReference(newCache))
        newCache
      }
    }

}
