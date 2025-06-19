// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object ArrayAccessParser {

  /**
   * Note: The following case could result in an unintended parsed AST, but we are strictly following how ralphc parses it.
   *
   * Imagine the following case:
   * {{{
   *   let array = [0, 1, 2]
   *   let copy = array
   *   [1] // This could be an array index-access, or it could also be a new array with a single element?
   * }}}
   *
   * But since ralphc errors when accessing arrays (`[1]`) without usage, we will also parse it similarly.
   *  - In Scala: Index access must be on the same line.
   *  - In Java: Index access can be on a new line.
   */
  def parseOrFail[Unknown: P]: P[SoftAST.ArrayAccess] =
    P {
      Index ~
        IdentifierParser.parseOrFail ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.OpenBracket) ~
        SpaceParser.parseOrFail.? ~
        ExpressionParser.parse ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parse(Token.BlockBracket) ~
        Index
    } map {
      case (from, identifier, preOpenBracketSpace, openBracket, preTypeSpace, accessIndex, preCloseBracketSpace, closeBracket, to) =>
        SoftAST.ArrayAccess(
          index = range(from, to),
          identifier = identifier,
          preOpenBracketSpace = preOpenBracketSpace,
          openBracket = openBracket,
          preAccessIndex = preTypeSpace,
          accessIndex = accessIndex,
          preCloseBracketSpace = preCloseBracketSpace,
          closeBracket = closeBracket
        )
    }

}
