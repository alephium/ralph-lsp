/*
 * Copyright 2021 Simer JS Plaha (simer.j@gmail.com - @simerplaha)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

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
