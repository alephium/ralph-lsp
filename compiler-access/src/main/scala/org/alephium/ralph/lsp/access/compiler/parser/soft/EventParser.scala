package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object EventParser {

  def parseOrFail[Unknown: P]: P[SoftAST.Event] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Event) ~
        SpaceParser.parse ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        ParameterParser.parse ~
        Index
    } map {
      case (from, eventToken, preIdentifierSpace, identifier, preParamSpace, params, to) =>
        SoftAST.Event(
          index = range(from, to),
          eventToken = eventToken,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params
        )
    }

}
