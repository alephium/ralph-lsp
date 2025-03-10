// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object BlockParser {

  /**
   * Parses a block's body such a parent block is already defined.
   * For example, within a parent contract or a function.
   *
   * @return Parsed content of a block.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.Block] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.OpenCurly) ~
        BlockPartParser.parseOrFail(blockExpressions = false, stop = Token.CloseCurly).rep ~
        TokenParser.parse(Token.CloseCurly) ~
        Index
    } map {
      case (from, openCurly, parts, closeCurly, to) =>
        SoftAST.Block(
          index = range(from, to),
          openCurly = openCurly,
          parts = parts,
          closeCurly = closeCurly
        )
    }

}
