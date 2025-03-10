// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.Type

import scala.collection.immutable.ArraySeq

object PrimitiveTypesCompleter {

  /**
   * Suggests all Ralph primitive types.
   *
   * @return An array sequence of primitive types.
   */
  def suggest(): ArraySeq[Suggestion.PrimitiveType] =
    ArraySeq(
      Suggestion.PrimitiveType(Type.U256),
      Suggestion.PrimitiveType(Type.I256),
      Suggestion.PrimitiveType(Type.Bool),
      Suggestion.PrimitiveType(Type.ByteVec),
      Suggestion.PrimitiveType(Type.Address)
    )

}
