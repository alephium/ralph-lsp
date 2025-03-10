// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object SpaceParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Space] =
    P(CodeParser.parseOrFail(TokenParser.WhileInOrFail(Token.spaces).!)) map {
      text =>
        SoftAST.Space(text)
    }

  def parseOrFailSingleLine[Unknown: P]: P[SoftAST.Space] =
    P(CodeParser.parseOrFail(TokenParser.WhileInOrFail(Token.inlineSpaces).!)) map {
      text =>
        SoftAST.Space(text)
    }

}
