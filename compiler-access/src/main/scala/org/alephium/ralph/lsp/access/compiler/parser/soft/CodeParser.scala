package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object CodeParser {

  def parseOrFail[Unknown: P, T <: Token](token: T): P[SoftAST.CodeToken[T]] =
    P(Index ~ token.lexeme ~ Index) map {
      case (from, to) =>
        SoftAST.CodeToken(
          index = range(from, to),
          token = token
        )
    }

  def parseOrFail[Unknown: P](parser: => P[String]): P[SoftAST.CodeString] =
    P(Index ~ parser ~ Index) map {
      case (from, code, to) =>
        SoftAST.CodeString(
          index = range(from, to),
          text = code
        )
    }

}
