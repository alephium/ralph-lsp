// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object AlphParser {

  def parseOrFail[Unknown: P]: P[SoftAST.TokenExpression[Token.PrimitiveUnit]] =
    P(alph ~ TokenParser.isBoundary()) map (SoftAST.TokenExpression(_))

  private def alph[Unknown: P]: P[SoftAST.TokenDocumented[Token.PrimitiveUnit]] =
    P {
      TokenParser.parseOrFailOneOf(
        prefixCheck = false,
        tokens = Iterator(Token.AlphUppercase, Token.AlphLowercase)
      )
    }

}
