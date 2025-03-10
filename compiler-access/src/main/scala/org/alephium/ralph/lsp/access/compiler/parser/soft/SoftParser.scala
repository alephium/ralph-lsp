// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

object SoftParser {

  def parse[Unknown: P]: P[SoftAST.RootBlock] =
    P(Start ~ RootBlockParser.parseOrFail ~ End)

}
