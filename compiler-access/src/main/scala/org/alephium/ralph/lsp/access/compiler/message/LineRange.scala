// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.message

object LineRange {

  val zero: LineRange =
    LineRange(
      from = LinePosition.zero,
      to = LinePosition.zero
    )

}

case class LineRange(
    from: LinePosition,
    to: LinePosition)
