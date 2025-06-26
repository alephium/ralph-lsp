// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object MapAssignmentParser {

  /**
   * Parses the syntax:
   * {{{
   *   mapping[Key, Value] map
   * }}}
   */
  def parseOrFail[Unknown: P]: P[SoftAST.MapAssignment] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Mapping) ~
        SpaceParser.parseOrFail.? ~
        TypeParamParser.parse ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        Index
    } map {
      case (from, mapping, preTypesSpace, types, preIdentifierSpace, identifier, to) =>
        SoftAST.MapAssignment(
          index = range(from, to),
          mapping = mapping,
          preTypesSpace = preTypesSpace,
          types = types,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier
        )
    }

}
