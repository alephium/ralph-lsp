// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object SpaceParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Space] =
    P(CodeParser.parseOrFail(TokenParsers.parse(Token.spaces, true).rep(1).!)) map {
      text =>
        SoftAST.Space(text)
    }

  def parseOrFailSingleLine[Unknown: P]: P[SoftAST.Space] =
    P(CodeParser.parseOrFail(TokenParsers.parse(Token.inlineSpaces, true).rep(1).!)) map {
      text =>
        SoftAST.Space(text)
    }

}
