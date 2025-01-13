package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object BooleanParser {

  def parseOrFail[Unknown: P]: P[SoftAST.TokenDocumented[Token.PrimitiveBoolean]] =
    P(TokenParser.parseOrFail(Token.True) | TokenParser.parseOrFail(Token.False))

  def parseOrFailExpression[Unknown: P]: P[SoftAST.TokenExpression[Token.PrimitiveBoolean]] =
    P(parseOrFail) map (SoftAST.TokenExpression(_))

}
