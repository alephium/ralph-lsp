// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.message

object LinePosition {

  val zero: LinePosition =
    LinePosition(line = 0, character = 0)

}

case class LinePosition(
    line: Int,
    character: Int)
