// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object RootBlockParser {

  /**
   * Parses a block's body such that a virtual block (curly braces), i.e. `{}` is defined.
   *
   * This parse is invoked only for the root parser call.
   *
   * To parse a block's body when a parent block is already defined, for example,
   * within a contract or a function, use [[BlockParser.parseOrFail]].
   *
   * @return Parsed content of a block.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.RootBlock] =
    P {
      Index ~
        BlockPartParser.parseOrFail(blockExpressions = true).rep ~
        Index
    } map {
      case (from, parts, to) =>
        SoftAST.RootBlock(
          index = range(from, to),
          parts = parts
        )
    }

}
