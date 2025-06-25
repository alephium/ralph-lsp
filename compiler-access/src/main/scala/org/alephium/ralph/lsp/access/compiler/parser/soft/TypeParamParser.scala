// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.point
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object TypeParamParser {

  def parse[Unknown: P]: P[SoftAST.TypeParamsExpectedAST] =
    P(Index ~ parseOrFail.?) map {
      case (_, Some(token)) =>
        token

      case (from, None) =>
        SoftAST.TypeParamsExpected(point(from))
    }

  /**
   * Parses type parameters using [[ArrayParser]] as a temporary solution.
   *
   * For better diagnostics, this should really be implemented with a parser specifically targeting
   * type parameters instead of reusing array parsing.
   *
   * Parses the syntax:
   * {{{
   *   mapping[Key, Value]
   *          ↑__________↑
   * }}}
   */
  private def parseOrFail[Unknown: P]: P[SoftAST.ArrayAST] =
    ArrayParser.parseOrFail

}
