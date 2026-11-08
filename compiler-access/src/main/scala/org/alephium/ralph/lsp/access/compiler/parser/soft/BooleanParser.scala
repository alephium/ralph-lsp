// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object BooleanParser {

  def parseOrFail[Unknown: P]: P[SoftAST.TokenExpression[Token.PrimitiveBoolean]] =
    P(boolean ~ TokenParser.isBoundary()) map (SoftAST.TokenExpression(_))

  private def boolean[Unknown: P]: P[SoftAST.TokenDocumented[Token.PrimitiveBoolean]] =
    P {
      TokenParser.parseOrFailWith(TokenParsers.parse(Token.booleans, false))
    }

}
