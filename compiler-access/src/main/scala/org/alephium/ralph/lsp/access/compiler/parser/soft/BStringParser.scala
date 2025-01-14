package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object BStringParser {

  def parseOrFail[Unknown: P]: P[SoftAST.BString] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.B) ~
        SpaceParser.parseOrFail.? ~
        TokenParser.parseOrFail(Token.Tick) ~
        TextParser.parseOrFail(Token.Tick).? ~
        TokenParser.parse(Token.Tick) ~
        Index
    } map {
      case (from, b, postBSpace, startTick, text, endTick, to) =>
        SoftAST.BString(
          index = range(from, to),
          b = b,
          postBSpace = postBSpace,
          startTick = startTick,
          text = text,
          endTick = endTick
        )
    }

}
