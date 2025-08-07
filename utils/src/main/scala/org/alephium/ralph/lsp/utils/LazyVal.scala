// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils

object LazyVal {

  def apply[A](f: => A): LazyVal[A] =
    new LazyVal[A](() => f, None)

}

/**
 * A lazily initialised value holder for managing `SoftAST` in `SourceCodeState`.
 *
 * @param load  Function that computes the value.
 * @param value The cached value, initially `None`, updated to `Some` once loaded.
 * @tparam A The type of the value.
 */
class LazyVal[A] private (
    load: () => A,
    @volatile private var value: Option[A]) {

  def isDefined: Boolean =
    value.isDefined

  def isEmpty: Boolean =
    value.isEmpty

  def fetch(): A =
    this.value match {
      case None =>
        val value = load()
        this.value = Some(value)
        value

      case Some(value) =>
        value
    }

  override def equals(that: Any): Boolean =
    that match {
      case that: LazyVal[A] @unchecked =>
        this.value == that.value

      case _ =>
        false
    }

  override def hashCode(): Int =
    value.hashCode()

}
