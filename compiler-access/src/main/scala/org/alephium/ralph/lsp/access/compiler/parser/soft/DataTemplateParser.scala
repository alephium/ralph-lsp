package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object DataTemplateParser {

  def parseOrFail[Unknown: P]: P[SoftAST.DataTemplate] =
    P {
      Index ~
        (TokenParser.parseOrFail(Token.Struct) | TokenParser.parseOrFail(Token.Enum) | TokenParser.parseOrFail(Token.Event)) ~
        SpaceParser.parse ~
        IdentifierParser.parse ~
        SpaceParser.parseOrFail.? ~
        ParameterParser.parse ~
        Index
    } map {
      case (from, templateType, preIdentifierSpace, identifier, preParamSpace, params, to) =>
        SoftAST.DataTemplate(
          index = range(from, to),
          dataType = templateType,
          preIdentifierSpace = preIdentifierSpace,
          identifier = identifier,
          preParamSpace = preParamSpace,
          params = params
        )
    }

}
