// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object UnresolvedParser {

  def parseOrFail[Unknown: P](stop: Token*): P[SoftAST.Unresolved] =
    P {
      Index ~
        CommentParser.parseOrFail.? ~
        CodeParser.parseOrFail(TokenParser.WhileNotOrFail(stop).!) ~
        Index
    } map {
      case (from, comment, text, to) =>
        SoftAST.Unresolved(
          index = range(from, to),
          documentation = comment,
          code = text
        )
    }

  def parseOrFailSpaceDelimited[Unknown: P](stop: Seq[Token]): P[SoftAST.Unresolved] =
    parseOrFail(stop ++ Token.spaces: _*)

}
