package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object TextParser {

  def parseOrFail[Unknown: P](stop: Token*): P[SoftAST.CodeString] =
    P(Index ~ TokenParser.WhileNotOrFail(stop: _*).! ~ Index) map {
      case (from, text, to) =>
        SoftAST.CodeString(
          text = text,
          index = range(from, to)
        )
    }

}
