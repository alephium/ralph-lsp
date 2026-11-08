// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token

object ReservedTokenParser {

  def parseOrFail[Unknown: P]: P[Token.Reserved] =
    TokenParsers.parse(Token.reserved.toSeq, false)

}
