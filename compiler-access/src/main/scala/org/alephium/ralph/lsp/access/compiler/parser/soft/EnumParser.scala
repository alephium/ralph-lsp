// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only
package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object EnumParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Enum] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Enum) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        // TODO: Narrow expressions parsed within an enum block.
        //       See issue: https://github.com/alephium/ralph-lsp/issues/387
        BlockParser.parseOrFail.? ~
        Index
    } map {
      case (from, enumToken, preIdentifierSpace, identifier, preParamSpace, block, to) =>
        SoftAST.Enum(
          index = range(from, to),
          enumToken = enumToken,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preBlockSpace = preParamSpace,
          block = block
        )
    }

}
